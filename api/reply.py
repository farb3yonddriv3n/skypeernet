import json
import command

def listpeers(state, obj):
    print(json.dumps(obj["payload"], indent=2, sort_keys=True))
    return 0

def message(state, obj):
    print(json.dumps(obj["payload"], indent=2, sort_keys=True))
    return 0

def listfiles(state, obj):
    print(json.dumps(obj["payload"], indent=2, sort_keys=True))
    return 0

cmds = [ { "command" : command.API_LISTPEERS, "func" : listpeers },
         { "command" : command.API_MESSAGE,   "func" : message   },
         { "command" : command.API_LISTFILES, "func" : listfiles } ]

def run(state, obj):
    try:
        for c in cmds:
            if c["command"] == obj["command"]:
                return c["func"](state, obj)
    except:
        print("Command failed")
    return 0
