import json
import command

def peeronline(state, obj):
    print(json.dumps(obj, indent=2, sort_keys=True))
    return 0

def peeroffline(state, obj):
    print(json.dumps(obj, indent=2, sort_keys=True))
    return 0

def listpeers(state, obj):
    state["peers"] = obj
    print(json.dumps(obj["payload"], indent=2, sort_keys=True))
    return 0

def message(state, obj):
    state["messages"].append(obj)
    print(json.dumps(obj["payload"], indent=2, sort_keys=True))
    return 0

def listfiles(state, obj):
    state["files"] = obj
    print(json.dumps(obj["payload"], indent=2, sort_keys=True))
    return 0

cmds = [ { "command" : command.API_LISTPEERS,    "func" : listpeers   },
         { "command" : command.API_MESSAGE,      "func" : message     },
         { "command" : command.API_LISTFILES,    "func" : listfiles   },
         { "command" : command.API_MESSAGE,      "func" : message     },
         { "command" : command.API_PEER_ONLINE,  "func" : peeronline  },
         { "command" : command.API_PEER_OFFLINE, "func" : peeroffline },
       ]

def run(state, obj):
    try:
        for c in cmds:
            if c["command"] == obj["command"]:
                return c["func"](state, obj)
        return -1
    except:
        return -1
