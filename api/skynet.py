import asyncio
import command

class skynet:
    def __init__(self, state):
        self.state = state
        return

    async def update(self):
        return 0

    async def run(self):
        while True:
            await asyncio.sleep(5)
            u = await self.update()
            if u != 0: break
        await self.run()
