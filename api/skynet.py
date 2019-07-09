import asyncio
import command

class skynet:
    def __init__(self, state):
        self.state = state
        self.actions = [
        { "blocking" : None,            "func" : command.listfiles_local,  "params" : None,
            "done"   : None, "request_id" : -1, "name" : "filesl" },
        { "blocking" : None,            "func" : command.listfiles_remote, "params" : None,
            "done"   : None, "request_id" : -1, "name" : "filesr" },
        { "blocking" : None,            "func" : command.jobadd, "params" : self.jobadd_file,
            "done"   : None, "request_id" : -1, "name" : "jobadd" },
        { "blocking" : None,           "func" : command.tshare,  "params" : self.jobadd_file,
            "done"   : None, "request_id" : -1, "name" : "tshare" },
        { "blocking" : self.is_mining, "func" : command.bmine,   "params" : None,
            "done"   : None, "request_id" : -1, "name" : "bmine"  },
        { "blocking" : None,           "func" : command.badv,    "params" : None,
            "done"   : None, "request_id" : -1, "name" : "badv"   },
        ]

    def request_id(self):
        return self.state["request"]

    def file_local_exists(self, remote):
        for l in self.state["files"]["local"]:
            if l["name"] == remote:
                return 1
        return 0

    def jobadd_file(self):
        for r in self.state["files"]["remote"]:
            if r["downloaded"] == False and self.file_local_exists(r["name"]) == 0:
                return [ None, r["name"] ]
        return []

    async def is_mining(self, action):
        action["request_id"] = self.request_id()
        r = await command.bmining(self.state, {})
        if r == -1: return -1
        return 0

    def reset(self):
        for a in self.actions:
            a["done"]       = None
            a["request_id"] = -1

    async def resume(self):
        for a in self.actions:
            if a["done"] != None and a["done"]["mining"] == False:
                if a == self.actions[-1]:
                    self.reset()
                    return await self.resume()
                continue
            if a["done"] != None and a["done"]["mining"] == True:
                return await a["blocking"](a)
            if a["done"] == None and a["request_id"] == -1:
                if a["params"]: params = a["params"]()
                else:           params = []
                if params == [] and a["params"]:
                    self.reset()
                    return
                a["request_id"] = self.request_id()
                r = await a["func"](self.state, params)
                if r == -1: return -1
                return

    def update(self):
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
