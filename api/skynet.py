import asyncio
import command

async def update(state):
    return 0

async def run(state):
    while True:
        await asyncio.sleep(5)
        u = await update(state)
        if u != 0: break
    await run(state)
