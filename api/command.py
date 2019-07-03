import json

API_LISTPEERS    = 0
API_MESSAGE      = 1
API_LISTFILES    = 2
API_PEER_ONLINE  = 3
API_PEER_OFFLINE = 4
API_JOBDUMP      = 5
API_JOBDONE      = 6
API_JOBADD       = 7
API_TSHARE       = 8

async def cmdsend(state, msg, cmd):
    msg["command"] = cmd
    obj = json.dumps(msg)
    r = await state["ftb"].write(obj)
    if len(r) != len(obj): return -1
    return 0

async def jobadd(state, params):
    msg = { "name" : params[1] }
    return await cmdsend(state, msg, API_JOBADD)

async def tshare(state, params):
    msg = { "name" : params[1] }
    return await cmdsend(state, msg, API_TSHARE)

async def message(state, params):
    msg = { "host"    : int(params[1]),
            "port"    : int(params[2]),
            "message" : params[3] }
    return await cmdsend(state, msg, API_MESSAGE)

async def listpeers(state, params):
    return await cmdsend(state, {}, API_LISTPEERS)

async def listfiles(state, params):
    return await cmdsend(state, {}, API_LISTFILES)

async def jobdump(state, params):
    return await cmdsend(state, {}, API_JOBDUMP)

commands = [ { "cmd" : "m",  "params" : 3, "func" : message   },
             { "cmd" : "l",  "params" : 0, "func" : listpeers },
             { "cmd" : "lf", "params" : 0, "func" : listfiles },
             { "cmd" : "jd", "params" : 0, "func" : jobdump   },
             { "cmd" : "ja", "params" : 1, "func" : jobadd    },
             { "cmd" : "ts", "params" : 1, "func" : tshare    },
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
