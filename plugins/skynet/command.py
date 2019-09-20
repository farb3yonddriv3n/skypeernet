import json

API_LISTPEERS        = 0
API_MESSAGE          = 1
API_LISTFILES_LOCAL  = 2
API_LISTFILES_REMOTE = 3
API_PEER_ONLINE      = 4
API_PEER_OFFLINE     = 5
API_JOBDUMP          = 6
API_JOBDONE          = 7
API_JOBADD           = 8
API_JOBFINALIZE      = 9
API_TSHARE           = 10
API_BMINE            = 11
API_BADVERTISE       = 12
API_BMINING          = 13
API_ROGUEDUMP        = 14
API_VERSIONDUMP      = 15
API_TRAFFICDUMP      = 16
API_TASKDUMP         = 17

async def cmdsend(state, msg, cmd):
    msg["command"] = cmd
    msg["request_id"] = state["request"]
    state["request"] += 1
    obj = json.dumps(msg)
    state["packets"]["sent"].append(msg)
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

async def listfiles_remote(state, params):
    return await cmdsend(state, {}, API_LISTFILES_REMOTE)

async def listfiles_local(state, params):
    return await cmdsend(state, {}, API_LISTFILES_LOCAL)

async def jobdump(state, params):
    return await cmdsend(state, {}, API_JOBDUMP)

async def bmine(state, params):
    return await cmdsend(state, {}, API_BMINE)

async def badv(state, params):
    return await cmdsend(state, {}, API_BADVERTISE)

async def bmining(state, params):
    return await cmdsend(state, {}, API_BMINING)

async def roguedump(state, params):
    return await cmdsend(state, {}, API_ROGUEDUMP)

async def versiondump(state, params):
    return await cmdsend(state, {}, API_VERSIONDUMP)

commands = [ { "cmd" : "m",   "params" : 3, "func" : message          },
             { "cmd" : "l",   "params" : 0, "func" : listpeers        },
             { "cmd" : "lfr", "params" : 0, "func" : listfiles_remote },
             { "cmd" : "lfl", "params" : 0, "func" : listfiles_local  },
             { "cmd" : "jd",  "params" : 0, "func" : jobdump          },
             { "cmd" : "ja",  "params" : 1, "func" : jobadd           },
             { "cmd" : "ts",  "params" : 1, "func" : tshare           },
             { "cmd" : "bm",  "params" : 0, "func" : bmine            },
             { "cmd" : "ba",  "params" : 0, "func" : badv             },
             { "cmd" : "bmn", "params" : 0, "func" : bmining          },
             { "cmd" : "rd",  "params" : 0, "func" : roguedump        },
             { "cmd" : "v",   "params" : 0, "func" : versiondump      },
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
