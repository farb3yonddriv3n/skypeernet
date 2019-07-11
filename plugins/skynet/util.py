import json
from pygments import highlight, lexers, formatters

def colorjson(obj):
    jstr = json.dumps(obj, indent=2, sort_keys=True)
    colorful = highlight(jstr, lexers.JsonLexer(), formatters.TerminalFormatter())
    print(colorful)

def fread(filename, mode):
    f = open(filename, mode)
    c = f.read()
    f.close()
    return c
