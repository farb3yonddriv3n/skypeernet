import asyncio, json, fcntl, os, errno, sys
from aiofile import AIOFile, Reader, Writer
import command, reply, skynet, util

FIFO_BTF = '/tmp/skypeernet_write'
FIFO_FTB = '/tmp/skypeernet_read'

async def piperecv(state):
    try:
        async with AIOFile(FIFO_BTF, 'rb') as fp:
            while True:
                msg = await fp.read(4096)
                if (len(msg) == 0):
                    break
                state["recvbuf"] += msg
                n = int.from_bytes(state["recvbuf"][:4], byteorder='little')
                if n > len(state["recvbuf"]): continue
                obj = json.loads(state["recvbuf"][4:n + 4].decode("utf-8"))
                state["recvbuf"] = state["recvbuf"][n + 4:]
                r = await reply.run(state, obj)
                if r != 0:
                    print("Command failed: ", obj, r)
    except:
        print("Exception")
    await piperecv(state)

async def stdinput(state):
    async with AIOFile('/dev/stdin', 'r') as f:
        while True:
            inpt = await f.read(256)
            if not inpt: break
            inpt = inpt.replace("\n", "")
            r = await command.run(state, inpt)
            if (r != 0): print('Command "%s" failed' % inpt)

async def ai(state):
    await state["skynet"].boot()

def get_config():
    filename = "config/skypeernet.cfg"
    if(len(sys.argv) == 2):
        filename = sys.argv[1]
    c = util.fread(filename, "r")
    return json.loads(c)

async def run(loop):
    try:
        try:
            os.mkfifo(FIFO_BTF)
            os.mkfifo(FIFO_FTB)
        except OSError as oe:
            if oe.errno != errno.EEXIST:
                raise
        ftb = await AIOFile(FIFO_FTB, 'w')
        state = { "cfg"      : get_config(),
                  "peers"    : {},
                  "files"    : { "remote" : [],
                                 "local"  : [] },
                  "messages" : [],
                  "jobs"     : {},
                  "packets"  : { "sent"    : [],
                                 "handled" : [] },
                  "recvbuf"  : bytes(),
                  "request"  : 0,
                  "ftb"      : ftb }
        state["skynet"] = skynet.skynet(state)
        task1 = loop.create_task(piperecv(state))
        task2 = loop.create_task(stdinput(state))
        task3 = loop.create_task(ai(state))
        await task1
        await task2
        await task3
    except OSError as e:
        raise("Exception: ", e)

def main():
    loop = asyncio.get_event_loop()
    loop.run_until_complete(run(loop))

main()
