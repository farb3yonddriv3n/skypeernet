import json
import command, util

def peeronline(state, obj):
    util.colorjson(obj)
    return 0

def peeroffline(state, obj):
    util.colorjson(obj)
    return 0

def listpeers(state, obj):
    state["peers"] = obj
    util.colorjson(obj)
    return 0

def message(state, obj):
    state["messages"].append(obj)
    #print(json.dumps(obj["payload"], indent=2, sort_keys=True))
    util.colorjson(obj)
    return 0

def listfiles_local(state, obj):
    files = []
    if "payload" in obj and "blocks" in obj["payload"]:
        for block in obj["payload"]["blocks"]:
            for transaction in block["transactions"]:
                files.append({ "name"       : transaction["name"],
                               "size"       : transaction["size"],
                               "downloaded" : transaction["downloaded"] })
                #for transaction in block["transactions"]:
                #    files.append({ "owner"      : root["owner"],
                #                   "name"       : transaction["name"],
                #                   "size"       : transaction["size"],
                #                   "downloaded" : transaction["downloaded"] })
    state["files"]["local"] = files
    util.colorjson(files)
    return 0

def listfiles_remote(state, obj):
    files = []
    if "payload" in obj and "roots" in obj["payload"]:
        for root in obj["payload"]["roots"]:
            for block in root["blocks"]:
                for transaction in block["transactions"]:
                    files.append({ "owner"      : root["owner"],
                                   "name"       : transaction["name"],
                                   "size"       : transaction["size"],
                                   "downloaded" : transaction["downloaded"] })
    state["files"]["remote"] = files
    util.colorjson(files)
    #util.colorjson(obj)
    return 0

def jobdump(state, obj):
    state["jobs"] = obj
    #print(json.dumps(obj["payload"], indent=2, sort_keys=True))
    util.colorjson(obj)
    return 0

def jobdone(state, obj):
    util.colorjson(obj)
    return 0

def tshare(state, obj):
    util.colorjson(obj)
    return 0

def bmine(state, obj):
    util.colorjson(obj)
    return 0

cmds = [ { "command" : command.API_LISTPEERS,        "func" : listpeers        },
         { "command" : command.API_MESSAGE,          "func" : message          },
         { "command" : command.API_LISTFILES_LOCAL,  "func" : listfiles_local  },
         { "command" : command.API_LISTFILES_REMOTE, "func" : listfiles_remote },
         { "command" : command.API_MESSAGE,          "func" : message          },
         { "command" : command.API_PEER_ONLINE,      "func" : peeronline       },
         { "command" : command.API_PEER_OFFLINE,     "func" : peeroffline      },
         { "command" : command.API_JOBDUMP,          "func" : jobdump          },
         { "command" : command.API_JOBDONE,          "func" : jobdone          },
         { "command" : command.API_TSHARE,           "func" : tshare           },
         { "command" : command.API_BMINE,            "func" : bmine            },
       ]

async def run(state, obj):
    try:
        for c in cmds:
            if c["command"] == obj["command"]:
                if c["func"](state, obj) != 0: return -1
                state["packets"]["recv"].append(obj)
                return await state["skynet"].update()
        return -1
    except:
        return -1
