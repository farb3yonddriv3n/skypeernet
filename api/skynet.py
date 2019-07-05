import asyncio
import command

class skynet:
    def __init__(self, state):
        self.state = state
        self.actions = [
        { "blocking" : None,   "func" : command.jobadd, "params" : self.jobadd_file,
            "done"   : None, "request_id" : -1, "name" : "jobadd" },
        { "blocking" : None,   "func" : command.tshare, "params" : self.jobadd_file,
            "done"   : None, "request_id" : -1, "name" : "tshare" },
        { "blocking" : None, "func" : command.bmine,  "params" : None,
            "done"   : None, "request_id" : -1, "name" : "bmine" },
        { "blocking" : None,   "func" : command.badv,   "params" : None,
          "done"   : None, "request_id" : -1, "name" : "badv" },
        ]

    def jobadd_file(self):
        for f in self.state["files"]["remote"]:
            if f["downloaded"] == False:
                return [ None, f["name"] ]
        return []

    async def resume(self):
        for a in self.actions:
            if a["done"] != None:
                continue
            if a["done"] == None and a["request_id"] != -1:
                return
            elif a["done"] == None and a["request_id"] == -1:
                if a["params"]: params = a["params"]()
                else:           params = []
                if params == [] and a["params"]: return
                r = await a["func"](self.state, params)
                if r == -1: print("error")
                a["request_id"] = self.state["request"] - 1
                return

    async def update(self):
        for h in self.state["packets"]["handled"]:
            for a in self.actions:
                if h["id"] == a["request_id"]:
                    a["done"] = h
                    self.state["packets"]["handled"] = []
                    return 0
        return 0

    async def start(self):
        await self.resume()
        await self.loop()

    async def loop(self):
        while True:
            await asyncio.sleep(5)
            u = await self.resume()
            if u != 0: break
        await self.loop()
