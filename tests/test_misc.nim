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

  Bools = seq[int]
  Maps = object
    data: seq[Item]

  AB = object
    a: int
    b: int

  MapAb = object
    maps: AB

const toml = """title = "GroupTitle"
items = """ & "\"\"\"" & "\nX;test1\nY;test2\nZ;test3" & "\"\"\""

const toml62 = """
srcDir = "docsrc"
homeDir = "docs"
"""

const arrayDoubleComma = """
bools = [1 , , 0]
"""

const arrayNoComma = """
bools = [1 0]
"""

const mapDoubleComma = """
maps = {a=1,,b=1}
"""

const mapNoComma = """
maps = {a=1 b=1}
"""

proc parseItems(s: string): Items =
  for l in s.splitLines():
    if l != "":
      let parts = l.split(';')
      result.add Item(id: parts[0], txt: parts[1])

proc readValue*(r: var TomlReader, items: var Items)=
  let s = parseAsString(r)
  items = parseItems(s)

proc readValue*(r: var TomlReader, items: var Bools)=
  r.parseList:
    items.add r.parseInt(int)

proc readValue*(r: var TomlReader, items: var Maps)=
  parseTable(r, key):
    items.data.add Item(
      id: key,
      txt: r.parseAsString()
    )

suite "test misc":
  test "bug 63":
    let group = Toml.decode(toml, Group)
    check group.title == "GroupTitle"

  test "bug 62":
    let t = Toml.decode(toml62,  NbConfig)
    echo t # error here
    # if it can echo without crash, then it's ok
    check true

  test "parseList doubleComma":
    expect TomlError:
      let t = Toml.decode(arrayDoubleComma, Bools, "bools")
      discard t

  test "parseList no comma":
    expect TomlError:
      let t = Toml.decode(arrayNoComma, Bools, "bools")
      discard t

  test "parseTable doubleComma":
    expect TomlError:
      let t = Toml.decode(mapDoubleComma, Maps, "maps")
      discard t

  test "parseTable noComma":
    expect TomlError:
      let t = Toml.decode(mapNoComma, Maps, "maps")
      discard t

  test "parseRecord doubleComma":
    expect TomlError:
      let t = Toml.decode(mapDoubleComma, MapAb)
      discard t

  test "parseRecord noComma":
    expect TomlError:
      let t = Toml.decode(mapNoComma, MapAb)
      discard t
