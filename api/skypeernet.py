import asyncio, json, fcntl, os, errno
from aiofile import AIOFile, Reader, Writer
import command, reply

FIFO_BTF = '/tmp/skypeernet_write'
FIFO_FTB = '/tmp/skypeernet_read'

async def trecv(state):
    async with AIOFile(FIFO_BTF, 'r') as fp:
        while True:
            msg = await fp.read(128)
            if (len(msg) == 0):
                break
            state["recv"] += msg
            try:
                obj = json.loads(state["recv"])
                if (reply.run(state, obj) != 0):
                    print("Command failed")
                state["recv"] = ""
            except:
               continue
    await trecv(state)

async def read_input(state):
    async with AIOFile('/dev/stdin', 'r') as f:
        while True:
            inpt = await f.read(256)
            if not inpt: break
            inpt = inpt.replace("\n", "")
            r = await command.run(state, inpt)
            if (r != 0): print('command "%s" failed' % inpt)

async def run(loop):
    try:
        try:
            os.mkfifo(FIFO_BTF)
            os.mkfifo(FIFO_FTB)
        except OSError as oe:
            if oe.errno != errno.EEXIST:
                raise
        ftb = await AIOFile(FIFO_FTB, 'w')
        state = { "recv" : "",
                  "ftb"  : ftb }
        task1 = loop.create_task(trecv(state))
        task2 = loop.create_task(read_input(state))
        await task1
        await task2
    except OSError as e:
        raise("Exception: ", e)

def main():
    loop = asyncio.get_event_loop()
    loop.run_until_complete(run(loop))

main()
