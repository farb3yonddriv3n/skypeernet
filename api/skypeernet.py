import asyncio, json, fcntl, os, errno
from aiofile import AIOFile, Reader, Writer
import command, reply, skynet

FIFO_BTF = '/tmp/skypeernet_write'
FIFO_FTB = '/tmp/skypeernet_read'

async def piperecv(state):
    async with AIOFile(FIFO_BTF, 'r') as fp:
        while True:
            msg = await fp.read(4096)
            if (len(msg) == 0):
                break
            state["recv"] += msg
            try:
                obj = json.loads(state["recv"])
                if (reply.run(state, obj) != 0):
                    print("Command failed: ", obj)
                state["recv"] = ""
            except:
               continue
    await piperecv(state)

async def stdinput(state):
    async with AIOFile('/dev/stdin', 'r') as f:
        while True:
            inpt = await f.read(256)
            if not inpt: break
            inpt = inpt.replace("\n", "")
            r = await command.run(state, inpt)
            if (r != 0): print('command "%s" failed' % inpt)

async def ai(state):
    await skynet.run(state)

async def run(loop):
    try:
        try:
            os.mkfifo(FIFO_BTF)
            os.mkfifo(FIFO_FTB)
        except OSError as oe:
            if oe.errno != errno.EEXIST:
                raise
        ftb = await AIOFile(FIFO_FTB, 'w')
        state = { "peers"    : {},
                  "files"    : {},
                  "messages" : [],
                  "recv"     : "",
                  "ftb"      : ftb }
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
