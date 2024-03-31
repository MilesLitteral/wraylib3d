import re
import os
import json
import argparse

# This is a middleware app which helps condone translating the .lc files to .json
# in modern context. This module could be translated into the engine at any time.
# It is written to quickly condone LC -> JSON translation. Be warned, the engine
# *does* use it so it shouldn't be moved or deleted.
cwd = os.path.abspath(os.getcwd())

def entitiesFileToJSON (inputString):
    #Matches all Objects ({..})
    match = re.findall(r"(?=\{)([\s\S]*?)(?<=\})", inputString) # (?=\{)([\s\S]*?)(?<=\}) #{([\s\S]*?)}
    matches:list[str] = sorted(match)                           # match.sort()
    return (matches)

def myFunc(e):
  return e["classname"]

def entityRawToJSON(m, index, jsonExample:list):
    jsonExport  = {}
    matchCases  = re.findall(r"(?<=\")[^ \n\n].*?(?=\")", str(m[index]))
    x_enum      = enumerate(matchCases)

    #matches all data in Objects (".." "..")
    for idx, elem in x_enum:
        if idx % 2 or elem != StopIteration:
            lastElem  = elem
            idx, elem = next(x_enum)
            if lastElem == "origin":
                elements = elem.split(" ")
                jsonExport[lastElem] = {"x": eval(elements[0]), "y": eval(elements[1]), "z": eval(elements[2])}
            elif lastElem == "_color":
                elements = elem.split(" ")
                jsonExport[lastElem] = {"r": eval(elements[0]), "g": eval(elements[1]), "b": eval(elements[2])}
            elif lastElem == "light" or lastElem == "radius" or lastElem == "spawnflags" or lastElem == "angle" or lastElem == "ambient" or lastElem == "spawnflags" or lastElem == "random" or lastElem == "wait":
                jsonExport[lastElem] = eval(elem)
            else:
                jsonExport[lastElem] = elem
            jsonExample.append(jsonExport)
    sort = sorted(jsonExample, key=myFunc)
    return sort

def writeJSON(jsonOutputPath, js:list):
    #"C:/Users/Manda/OneDrive/Documents/Github/wraylib3d/.lc_q3.cache/q3dm1_python.json"
    f2 = open(cwd + jsonOutputPath, "w+")
    f2.write(json.dumps({"ecData": js}, indent=4, sort_keys=True))

def convertEntitiesToJSON(entitiesPath, outputPath):
    #"C:/Users/Manda/OneDrive/Documents/Github/wraylib3d/.lc_q3.cache/q3dm1.entities"
    jsonExample = []
    f           = open(cwd + entitiesPath, "r")
    match       = entitiesFileToJSON (f.read())

    for i in range(0, len(match)):
        entityRawToJSON(match, i, jsonExample)

    print(len(jsonExample))
    writeJSON(outputPath, jsonExample) # "C:/Users/Manda/OneDrive/Documents/Github/wraylib3d/.lc_q3.cache/q3dm1_python.json"
    print(jsonExample)
    print("Done")

parser = argparse.ArgumentParser(description="Convert .Entities to .JSON",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument("input",  help="Source      location")
parser.add_argument("output", help="Destination location")
args   = parser.parse_args()
config = vars(args)
print(config)

convertEntitiesToJSON(args.input, args.output)