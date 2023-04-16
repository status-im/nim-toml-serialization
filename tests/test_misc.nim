import
  strutils,
  ../toml_serialization,
  unittest2

type
  Item* = object
    id*: string
    txt*: string

  Items* = seq[Item]

  Group* = object
    title*: string
    items*: Items

  NbConfig* = object
    srcDir*, homeDir*: string

const toml = """title = "GroupTitle"
items = """ & "\"\"\"" & "\nX;test1\nY;test2\nZ;test3" & "\"\"\""

let toml62 = """
srcDir = "docsrc"
homeDir = "docs"
"""

proc parseItems(s: string): Items =
  for l in s.splitLines():
    if l != "":
      let parts = l.split(';')
      result.add Item(id: parts[0], txt: parts[1])

proc readValue*(r: var TomlReader, items: var Items)=
  let s = parseAsString(r)
  items = parseItems(s)

suite "test misc":
  test "bug 63":
    let group = Toml.decode(toml, Group)
    check group.title == "GroupTitle"

  test "bug 62":
    let t = Toml.decode(toml62,  NbConfig)
    echo t # error here
    # if it can echo without crash, then it's ok
    check true
