#ifndef CU_H_
#define CU_H_

#include "common.h"
#include "CUnit/Basic.h"

#define F64BIN   "test/files/file64kb.bin"
#define F128BIN  "test/files/file128kb.bin"
#define F256BIN  "test/files/file256kb.bin"
#define F512BIN  "test/files/file512kb.bin"
#define F1024BIN "test/files/file1024kb.bin"

#define A(m_a, m_b) (CU_ASSERT(m_b == m_a))

struct test_s {
    const char *description;
    void (*func)(void);
};

void t1_group_mine_block_append_transactions();
void t2_group_root_load_save();
void t3_block_append_transactions();
void t4_rsa_encrypt_decrypt();
void t5_root_compare();
void t6_packet();
void t7_list();

#endif
