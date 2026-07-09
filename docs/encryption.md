# Encryption redesign: per-file content keys (CEK)

## Problem

The legacy scheme derives the AES-256 file key from the sharer's RSA
*private* key (`SHA256(priv.pem)`). Anyone who wants to decrypt a shared
file needs the sharer's actual private key, which:

1. hands over the sharer's network identity (the same key answers
   `COMMAND_AUTH` challenges) — full impersonation;
2. decrypts *every* file that peer ever shared, not just the granted one;
3. cannot be revoked.

## Design

The identity RSA keypair (`.priv.key` / `.pub.key`) becomes
authentication-only and never leaves the machine. Every shared file gets
its own random AES-256 content key (CEK). The CEK — not the identity —
is what gets distributed, wrapped with the *recipients'* public keys.

```
SHARING (ta myfile.txt "tags" [to <pubkeyhash>,...])
────────────────────────────────────────────────────
CEK      = RAND_bytes(32)                    fresh per file, CSPRNG
chunks   = AES-256-GCM(file, CEK, timeiter IVs)   same chunk pipeline
filename = AES-256-GCM(name, CEK, random IV)      replaces RSA encx()

transaction carries "key slots":
  slot[0] = { to: SELF_pubkeyhash, key: RSA(self_pub,  CEK) }   always
  slot[N] = { to: peer_pubkeyhash, key: RSA(peer_pub, CEK) }   per grant

DOWNLOADING
───────────
find slot where to == my pubkeyhash
CEK = RSA_decrypt(my OWN .priv.key, slot.key)
decrypt filename, decrypt chunks
```

### Sharing modes

| Mode              | CLI                     | Who can read       | On chain            |
|-------------------|-------------------------|--------------------|---------------------|
| plain             | `ta f t plain`          | everyone           | nothing             |
| private (default) | `ta f t`                | only sharer        | one self slot       |
| targeted          | `ta f t to <h>,<h>`     | sharer + listed    | one slot per peer   |

"Public" sharing is `plain` — a published CEK is cryptographically
equivalent to no encryption, so no separate mode exists.

### On-chain format

The `fileadd` transaction JSON gains:

```json
"enc": {
  "scheme": 2,
  "slots": [ { "to": "<64-hex pubkeyhash>", "key": "<base64 RSA(CEK)>" } ]
}
```

Schemes: `0` = plain, `1` = legacy (privkey-derived AES), `2` = CEK.

Compatibility rules:

- A missing `enc` block loads as scheme 1 (legacy), so every existing
  block still loads and validates. The `keys/<pubkeyhash>.pem` private-key
  import path stays alive for reading old chains.
- `hash_meta` covers scheme + slots, but only for scheme != 1 (marker
  appended to the hash buffer), so legacy hashes are byte-identical.
  This makes slots tamper-evident: stripping or swapping a slot fails
  `validate()`.
- Non-upgraded peers reject scheme-2 blocks (they hash without the slot
  data). This is a network-upgrade release; gate on the transaction
  `version` field.

### Detail changes vs legacy

- GCM chunk tag: stored as plain base64 (a GCM tag is not secret);
  legacy RSA-wrapped tags (`encx`/`decx`) remain readable for scheme 1.
- Filename/description: AES-GCM under the CEK, stored as
  `base64(iv || tag || ciphertext)`; self-contained random IV so it can
  never collide with chunk IVs.
- CEK resolution is centralized (`file_cek_resolve`): plain → none,
  legacy → imported private key, CEK → unwrap own slot; result cached
  on `file_s`.

### Security properties

- Identity key never leaves the machine — no impersonation risk.
- Per-file blast radius: one leaked CEK exposes exactly one file.
- Granting file A reveals nothing about file B.
- Slots are integrity-bound to the mined block.
- Out of scope: un-sharing already-delivered bytes; protection of
  self-slots after a *future* identity-key compromise (inherent to
  non-ratcheting public-key schemes).

## Phases

1. **CEK core** (implemented): scheme field, CEK generation, chunk +
   filename encryption under CEK, self slot, finalize/decode
   resolution, legacy read path preserved.
   Touches: `file.h`, `file.c`, `transaction.h`, `transaction/file_add.c`,
   `job.c`, `utils.c`, `distfs.c`.
2. **Targeted slots** (implemented): `ta ... to <hashes>`; recipient
   public keys resolved from imported `keys/<pubkeyhash>.pub.pem` files
   first, then the live peer list (announce PEMs); `kx` exports own
   public key. Sharing a *public* key out-of-band replaces sharing a
   private key.
3. **Key send** (implemented): late grants without re-mining.
   `ks <filehash> <pubkeyhash>` sends `RSA(recipient_pub, CEK)` over the
   `COMMAND_KEY` payload to an online peer; the recipient verifies the
   key unwraps with its own private key, stores the wrapped blob as
   `keys/<filehash>.cek` (never overwriting an existing one), and
   `file_cek_resolve` consults that store when no chain slot matches.
   Anyone holding the CEK (sharer or grantee) can grant onward — the
   same power they'd have by just sending the decrypted file.
4. **Group keys** (implemented): a group key (GK) is a random symmetric
   AES-256 key identified by SHA256(GK), living in the same 64-hex
   namespace as pubkeyhashes. `kg` creates one (stored as
   `keys/<gkhash>.grp`, base64). `ta ... to <gkhash>` adds a group slot:
   `{to: gkhash, key: base64(iv || tag || AES-GCM(GK, CEK))}` — AES
   instead of RSA, distinguished purely by which hash the slot matches
   at resolve time. Membership = holding the GK: distributed over the
   wire via `ks <gkhash> <peer>` (`KEY_TYPE_GROUP`; receiver verifies
   SHA256(secret) == gkhash and hot-loads it, no restart) or offline by
   copying the `.grp` file. Any member decrypts all group files (past
   and future) and can re-share the GK — a shared-trust model; eviction
   is key rotation (new `kg`, share future files to the new hash).
