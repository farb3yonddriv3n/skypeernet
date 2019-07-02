import json

API_LISTPEERS    = 0
API_MESSAGE      = 1
API_LISTFILES    = 2
API_PEER_ONLINE  = 3
API_PEER_OFFLINE = 4
API_JOBDUMP      = 5

async def message(state, params):
    msg = { "command" : API_MESSAGE,
            "host"    : int(params[1]),
            "port"    : int(params[2]),
            "message" : params[3] }
    obj = json.dumps(msg)
    r = await state["ftb"].write(obj)
    if len(r) != len(obj): return -1
    return 0

async def listpeers(state, params):
    msg = { "command" : API_LISTPEERS }
    obj = json.dumps(msg)
    r = await state["ftb"].write(obj)
    if len(r) != len(obj): return -1
    return 0

async def listfiles(state, params):
    msg = { "command" : API_LISTFILES }
    obj = json.dumps(msg)
    r = await state["ftb"].write(obj)
    if len(r) != len(obj): return -1
    return 0

async def jobdump(state, params):
    msg = { "command" : API_JOBDUMP }
    obj = json.dumps(msg)
    r = await state["ftb"].write(obj)
    if len(r) != len(obj): return -1
    return 0

commands = [ { "cmd" : "m",  "params" : 3, "func" : message   },
             { "cmd" : "l",  "params" : 0, "func" : listpeers },
             { "cmd" : "lf", "params" : 0, "func" : listfiles },
             { "cmd" : "jd", "params" : 0, "func" : jobdump }
           ]

async def parse(state, inpt):
    params = inpt.split(" ")
    if len(params) < 1: return -1
    for c in commands:
        if c["cmd"] == params[0] and c["params"] == len(params) - 1:
            return await c["func"](state, params)
    return -1

async def run(state, inpt):
    return await parse(state, inpt)
