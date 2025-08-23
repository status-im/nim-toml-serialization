# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  std/[os, options, strutils, tables, typetraits],
  unittest2,
  ../toml_serialization,
  ../toml_serialization/[lexer, value_ops]

type
  Owner = object
    name: string
    dob: TomlDateTime
    cap: float64
    dobString: string
    dobToml: TomlValueRef

  Database = object
    server: string
    ports: array[3, int]
    connection_max: int
    enabled: bool
    negative: int

  ConfigCap = enum
    TomlCap
    JsonCap

  Fruits = enum
    Apple = "apple"
    Banana = "banana"

  Example = object
    title: string
    owner: Owner
    database: Database
    hosts: seq[string]
    configcap: ConfigCap
    cc: ConfigCap
    fruit: Fruits

  GrandChild = object
    name: string
    age: int

  ChildObject = object
    name: string
    child: GrandChild

  NestedObject = object
    child: ChildObject
    son: GrandChild

{.warning[HoleEnumConv]:off.}

ConfigCap.configureTomlDeserialization(
  allowNumericRepr = true)

proc readValue(r: var TomlReader, table: var Table[string, int]) =
  parseTable(r, key):
    table[key] = r.parseInt(int)

type EnumTestX = enum
  x0,
  x1,
  x2

type EnumTestY = enum
  y1 = 1,
  y3 = 3,
  y4,
  y6 = 6
EnumTestY.configureTomlDeserialization(
  allowNumericRepr = true)

type EnumTestZ = enum
  z1 = "aaa",
  z2 = "bbb",
  z3 = "ccc"

type EnumTestN = enum
  n1 = "aaa",
  n2 = "bbb",
  n3 = "ccc"
EnumTestN.configureTomlDeserialization(
  stringNormalizer = nimIdentNormalize)

type EnumTestO = enum
  o1,
  o2,
  o3
EnumTestO.configureTomlDeserialization(
  allowNumericRepr = true,
  stringNormalizer = nimIdentNormalize)

{.warning[HoleEnumConv]:on.}

type CaseObject = object
  case kind: Fruits
  of Apple: appleVal: int
  of Banana: bananaVal: string

proc readValue(
    r: var TomlReader, value: var CaseObject)
    {.raises: [IOError, SerializationError].} =
  var
    kindSpecified = false
    paramsSpecified = false

  parseTable(r, key):
    case key
    of "kind":
      value = CaseObject(kind: r.readValue(Fruits))
      kindSpecified = true

    of "appleVal":
      if kindSpecified:
        case value.kind
        of Apple:
          r.readValue(value.appleVal)
        else:
          r.lex.raiseTomlErr(
            "The '" & key & "' field is not allowed for '" & $value.kind & "'")
      else:
        r.lex.raiseTomlErr(
          "The '" & key & "' field must be specified after the 'kind' field")
      paramsSpecified = true

    of "bananaVal":
      if kindSpecified:
        case value.kind
        of Banana:
          r.readValue(value.bananaVal)
        else:
          r.lex.raiseTomlErr(
            "The 'bananaVal' field is not allowed for '" & $value.kind & "'")
      else:
        r.lex.raiseTomlErr(
          "The '" & key & "' field must be specified after the 'kind' field")
      paramsSpecified = true

    else:
      r.lex.raiseUnexpectedField(key, typeof(value).name)

  if not (kindSpecified and paramsSpecified):
    r.lex.raiseTomlErr(
      "The CaseObject value should have sub-fields named " &
      "'kind', and 'appleVal' or 'bananaVal' depending on 'kind'")

suite "test decoder":
  let rawToml = readFile("tests" / "tomls" / "example.toml")
  var ex = Toml.decode(rawToml, Example)

  var x: TomlDateTime
  x.date = some(TomlDate(
    year: 1979, month: 5, day:27
    ))
  x.time = some(TomlTime(hour: 7, minute: 32, second: 0, subsecond: 0))
  x.zone = some(TomlTimeZone(
    positiveShift: false,
    hourShift: 8,
    minuteShift: 0
    ))

  test "basic example":
    check:
      ex.title == "TOML Example"
      ex.owner.name == "Tom Preston-Werner"
      ex.owner.dob == x
      ex.owner.dobString == "1979-05-27T07:32:00-08:00"
      ex.owner.dobToml.kind == TomlKind.DateTime
      ex.database.server == "192.168.1.1"
      ex.database.ports[0] == 8001
      ex.database.ports[1] == 8002
      ex.database.ports[2] == 8003
      ex.database.connection_max == 5000
      ex.database.enabled == true
      ex.database.negative == -3
      ex.hosts[0] == "alpha"
      ex.hosts[1] == "omega"
      ex.configcap == JsonCap
      ex.cc == TomlCap
      ex.fruit == Banana

  test "spec example":
    discard Toml.loadFile("tests" / "tomls" / "example.toml", TomlValueRef)
    discard Toml.loadFile("tests" / "tomls" / "spec.toml", TomlValueRef)

  test "nested object":
    var x = Toml.loadFile("tests" / "tomls" / "nested_object.toml", NestedObject)
    check:
      x.child.name == "TOML"
      x.child.child.name == "CHILD"
      x.child.child.age == 10
      x.son.name == "SON"

  type
    NoField = object
      name: string
      weight: int
      age: int

    OptionalField = object
      name: string
      age: Option[int]

  test "nofield":
    var x = Toml.decode("name=\"X\" \n age=10", NoField)
    check x.age == 10
    check x.name == "X"
    check x.weight == 0

    expect TomlError:
      discard Toml.decode("name=\"X\" \n age=10 \n banana = 10", NoField)

    var z = Toml.decode("name=\"X\" \n age=10", OptionalField)
    check z.name == "X"
    check z.age.isSome
    check z.age.get() == 10

    var w = Toml.decode("name=\"X\"", OptionalField)
    check w.name == "X"
    check w.age.isNone

  test "bad toml":
    type
      SubObject = object
        name: string

      TopObject = object
        child: SubObject

    expect TomlError:
      discard Toml.decode("child = name = \"Toml\"",  TopObject)

  test "trailing comma in array":
    type
      XX = array[3, int]
      YY = seq[int]

      TrailingComma = object
        x: seq[int]
        y: array[3, int]
        w: TomlValueRef
        s: string
        z: string

    let x = Toml.decode("x = [1, 2, 3]", XX, "x")
    check x == [1, 2, 3]

    let y = Toml.decode("x = [1, 2, 3]", YY, "x")
    check y == @[1, 2, 3]

    let xx = Toml.decode("x = [1, 2, 3, ]", XX, "x")
    check xx == [1, 2, 3]

    let yy = Toml.decode("x = [1, 2, 3, ]", YY, "x")
    check yy == @[1, 2, 3]

    let ww = Toml.decode("x = [7, 8, 9, ]", TomlValueRef, "x")

    const doc = """
      x = [1, 2, 3, ]
      y = [4,5,6,]
      w = [7,8,9,]
      s = [10 , 11 , 12 , ]
      z = "ok" # this last entry is to make sure we parse past last entry ']'
    """

    let zz = Toml.decode(doc, TrailingComma)
    check zz.x == @[1, 2, 3]
    check zz.y == [4, 5, 6]
    check zz.w == ww
    check zz.s == "[10,11,12]"
    check zz.z == "ok"

  test "Trailing comma in inline table":
    type
      AbcTC = object
        a: int
        b: int
        c: int

      TrailingCommaIT = object
        x: TomlValueRef
        y: string
        z: AbcTC
        w: Table[string, int]
        s: string

    const doc = """
      x = {a=1, b=2, c=3, }
      y = {a=1, b=2, c=3, }
      z = {a=1, b=2, c=3, }
      w = {a=1, b=2, c=3, }
      s = "ok"
    """

    let xx = Toml.decode("x = {a=1, b=2, c=3, }", TomlValueRef, "x", {TomlInlineTableTrailingComma})
    let r = Toml.decode(doc, TrailingCommaIT, {TomlInlineTableTrailingComma})
    check r.x == xx
    check r.y == "{a=1,b=2,c=3}"
    check r.z == AbcTC(a:1,b:2,c:3)
    check r.w["a"] == 1
    check r.w["b"] == 2
    check r.w["c"] == 3
    check r.s == "ok"

  test "case object":
    let toml = """
    [apple]
      kind = "apple"
      appleVal = 123

    [banana]
      kind = "banana"
      bananaVal = "Hello Banana"
    """

    let apple = Toml.decode(toml, CaseObject, "apple")
    check apple.appleVal == 123

    let banana = Toml.decode(toml, CaseObject, "banana")
    check banana.bananaVal == "Hello Banana"

  test "enums":
    check:
      Toml.decode("v = \"x0\"\n", EnumTestX, "v") == x0
      Toml.decode("v = \"x1\"\n", EnumTestX, "v") == x1
      Toml.decode("v = \"x2\"\n", EnumTestX, "v") == x2
      Toml.decode("v = \'x0\'\n", EnumTestX, "v") == x0
      Toml.decode("v = \'x1\'\n", EnumTestX, "v") == x1
      Toml.decode("v = \'x2\'\n", EnumTestX, "v") == x2
    expect TomlError:
      discard Toml.decode("v = 0\n", EnumTestX, "v")
    expect TomlError:
      discard Toml.decode("v = 1\n", EnumTestX, "v")
    expect TomlError:
      discard Toml.decode("v = 2\n", EnumTestX, "v")
    expect TomlError:
      discard Toml.decode("v = 3\n", EnumTestX, "v")
    expect TomlError:
      discard Toml.decode("v = \"X0\"\n", EnumTestX, "v")
    expect TomlError:
      discard Toml.decode("v = \"X1\"\n", EnumTestX, "v")
    expect TomlError:
      discard Toml.decode("v = \"X2\"\n", EnumTestX, "v")
    expect TomlError:
      discard Toml.decode("v = \"x_0\"\n", EnumTestX, "v")
    expect TomlError:
      discard Toml.decode("v = \"\"\n", EnumTestX, "v")
    expect TomlError:
      discard Toml.decode("v = \"0\"\n", EnumTestX, "v")

    check:
      Toml.decode("v = \"y1\"\n", EnumTestY, "v") == y1
      Toml.decode("v = \"y3\"\n", EnumTestY, "v") == y3
      Toml.decode("v = \"y4\"\n", EnumTestY, "v") == y4
      Toml.decode("v = \"y6\"\n", EnumTestY, "v") == y6
      Toml.decode("v = \'y1\'\n", EnumTestY, "v") == y1
      Toml.decode("v = \'y3\'\n", EnumTestY, "v") == y3
      Toml.decode("v = \'y4\'\n", EnumTestY, "v") == y4
      Toml.decode("v = \'y6\'\n", EnumTestY, "v") == y6
      Toml.decode("v = 1\n", EnumTestY, "v") == y1
      Toml.decode("v = 3\n", EnumTestY, "v") == y3
      Toml.decode("v = 4\n", EnumTestY, "v") == y4
      Toml.decode("v = 6\n", EnumTestY, "v") == y6
    expect TomlError:
      discard Toml.decode("v = 0\n", EnumTestY, "v")
    expect TomlError:
      discard Toml.decode("v = 2\n", EnumTestY, "v")
    expect TomlError:
      discard Toml.decode("v = 5\n", EnumTestY, "v")
    expect TomlError:
      discard Toml.decode("v = 7\n", EnumTestY, "v")
    expect TomlError:
      discard Toml.decode("v = \"Y1\"\n", EnumTestY, "v")
    expect TomlError:
      discard Toml.decode("v = \"Y3\"\n", EnumTestY, "v")
    expect TomlError:
      discard Toml.decode("v = \"Y4\"\n", EnumTestY, "v")
    expect TomlError:
      discard Toml.decode("v = \"Y6\"\n", EnumTestY, "v")
    expect TomlError:
      discard Toml.decode("v = \"y_1\"\n", EnumTestY, "v")
    expect TomlError:
      discard Toml.decode("v = \"\"\n", EnumTestY, "v")
    expect TomlError:
      discard Toml.decode("v = \"1\"\n", EnumTestY, "v")

    check:
      Toml.decode("v = \"aaa\"\n", EnumTestZ, "v") == z1
      Toml.decode("v = \"bbb\"\n", EnumTestZ, "v") == z2
      Toml.decode("v = \"ccc\"\n", EnumTestZ, "v") == z3
      Toml.decode("v = \'aaa\'\n", EnumTestZ, "v") == z1
      Toml.decode("v = \'bbb\'\n", EnumTestZ, "v") == z2
      Toml.decode("v = \'ccc\'\n", EnumTestZ, "v") == z3
    expect TomlError:
      discard Toml.decode("v = 0\n", EnumTestZ, "v")
    expect TomlError:
      discard Toml.decode("v = \"AAA\"\n", EnumTestZ, "v")
    expect TomlError:
      discard Toml.decode("v = \"BBB\"\n", EnumTestZ, "v")
    expect TomlError:
      discard Toml.decode("v = \"CCC\"\n", EnumTestZ, "v")
    expect TomlError:
      discard Toml.decode("v = \"z1\"\n", EnumTestZ, "v")
    expect TomlError:
      discard Toml.decode("v = \"a_a_a\"\n", EnumTestZ, "v")
    expect TomlError:
      discard Toml.decode("v = \"\"\n", EnumTestZ, "v")
    expect TomlError:
      discard Toml.decode("v = \"\ud83d\udc3c\"\n", EnumTestZ, "v")

    check:
      Toml.decode("v = \"aaa\"\n", EnumTestN, "v") == n1
      Toml.decode("v = \"bbb\"\n", EnumTestN, "v") == n2
      Toml.decode("v = \"ccc\"\n", EnumTestN, "v") == n3
      Toml.decode("v = \"aAA\"", EnumTestN, "v") == n1
      Toml.decode("v = \"bBB\"", EnumTestN, "v") == n2
      Toml.decode("v = \"cCC\"", EnumTestN, "v") == n3
      Toml.decode("v = \"a_a_a\"", EnumTestN, "v") == n1
      Toml.decode("v = \"b_b_b\"", EnumTestN, "v") == n2
      Toml.decode("v = \"c_c_c\"", EnumTestN, "v") == n3
    expect TomlError:
      discard Toml.decode("v = 0\n", EnumTestN, "v")
    expect TomlError:
      discard Toml.decode("v = \"AAA\"\n", EnumTestN, "v")
    expect TomlError:
      discard Toml.decode("v = \"BBB\"\n", EnumTestN, "v")
    expect TomlError:
      discard Toml.decode("v = \"CCC\"\n", EnumTestN, "v")
    expect TomlError:
      discard Toml.decode("v = \"Aaa\"\n", EnumTestN, "v")
    expect TomlError:
      discard Toml.decode("v = \"Bbb\"\n", EnumTestN, "v")
    expect TomlError:
      discard Toml.decode("v = \"Ccc\"\n", EnumTestN, "v")
    expect TomlError:
      discard Toml.decode("v = \"n1\"\n", EnumTestN, "v")
    expect TomlError:
      discard Toml.decode("v = \"_aaa\"\n", EnumTestN, "v")
    expect TomlError:
      discard Toml.decode("v = \"\"\n", EnumTestN, "v")
    expect TomlError:
      discard Toml.decode("v = \"\ud83d\udc3c\"\n", EnumTestN, "v")

    check:
      Toml.decode("v = \"o1\"\n", EnumTestO, "v") == o1
      Toml.decode("v = \"o2\"\n", EnumTestO, "v") == o2
      Toml.decode("v = \"o3\"\n", EnumTestO, "v") == o3
      Toml.decode("v = \"o_1\"", EnumTestO, "v") == o1
      Toml.decode("v = \"o_2\"", EnumTestO, "v") == o2
      Toml.decode("v = \"o_3\"", EnumTestO, "v") == o3
      Toml.decode("v = 0\n", EnumTestO, "v") == o1
      Toml.decode("v = 1\n", EnumTestO, "v") == o2
      Toml.decode("v = 2\n", EnumTestO, "v") == o3
    expect TomlError:
      discard Toml.decode("v = 3\n", EnumTestO, "v")
    expect TomlError:
      discard Toml.decode("v = \"O1\"\n", EnumTestO, "v")
    expect TomlError:
      discard Toml.decode("v = \"O2\"\n", EnumTestO, "v")
    expect TomlError:
      discard Toml.decode("v = \"O3\"\n", EnumTestO, "v")
    expect TomlError:
      discard Toml.decode("v = \"_o1\"\n", EnumTestO, "v")
    expect TomlError:
      discard Toml.decode("v = \"\"\n", EnumTestO, "v")
    expect TomlError:
      discard Toml.decode("v = \"\ud83d\udc3c\"\n", EnumTestO, "v")

suite "table array test suite":
  type
    Disc = object
      sector: int
      cylinder: int

    TableArray = object
      disc: seq[Disc]
      cd: array[3, Disc]

  const taFile = "tests" / "tomls" / "table-array.toml"

  test "table array basic decoder":
    let ta = Toml.loadFile(taFile, TableArray)
    check:
      ta.disc.len == 3
      ta.disc[2].sector == 7
      ta.disc[2].cylinder == 8
      ta.cd[0].sector == 9
      ta.cd[0].cylinder == 10

    type
      WantCD = object
        cd: seq[Disc]

    let cds = """
      [[cx]]
        sector = 11
        cylinder = 12

      [[cd]]
        sector = 13
        cylinder = 14
    """

    expect TomlError:
      discard Toml.decode(cds, WantCD)

    let z = Toml.decode(cds, WantCD, flags = {TomlUnknownFields})
    check:
      z.cd.len == 1
      z.cd[0].sector == 13
      z.cd[0].cylinder == 14

  test "table array keyed mode":
    let rawTa = readFile(taFile)
    let x = Toml.decode(rawTa, seq[Disc], "cd")
    check:
      x.len == 2
      x[0].sector == 9
      x[0].cylinder == 10
      x[1].sector == 11
      x[1].cylinder == 12

    let y = Toml.decode(rawTa, array[3, Disc], "disc")
    check:
      y[0].sector == 3
      y[0].cylinder == 4
      y[1].sector == 5
      y[1].cylinder == 6
      y[2].sector == 7
      y[2].cylinder == 8

    let z = Toml.loadFile(taFile, seq[Disc], "cd")
    check:
      z.len == 2
      z[0].sector == 9
      z[0].cylinder == 10
      z[1].sector == 11
      z[1].cylinder == 12

  test "table array optional fields":
    type
      Transition = enum
        noTransition
        crossFade

      Rgba8 = object
        r, g, b: uint8

      PointD = object
        x, y: float64

      Label = object
        face: Option[string]
        text: Option[string]
        color: Option[Rgba8]
        size: Option[float64]
        shadow: Option[bool]
        pos: Option[PointD]

      Scene = object
        background: Option[string]
        duration: Option[float32]
        labels: seq[Label]
        transition: Option[Transition]

      Song = object
        scenes: seq[Scene]

    const taof = "tests" / "tomls" / "table-array-optional-fields.toml"
    let p = Toml.loadFile(taof, Song, flags = {TomlInlineTableNewline})
    check p.scenes.len == 5

type
  ValidIpAddress {.requiresInit.} = object
    value: string

  TestObject = object
    address: Option[ValidIpAddress]

proc readValue(r: var TomlReader, value: var ValidIpAddress) =
  value.value = r.parseAsString()

suite "optional fields test suite":
  test "optional field with requiresInit pragma":
    when true:
      # Deserialization of `requiresInit` types worked only due to bugs:
      # https://github.com/status-im/nim-serialization/issues/104
      # A better solution needs to be found if toml is to support `requiresInit`
      # types.
      skip()
    else:
      {.warning[UnsafeDefault]:off.} # ValidIpAddress dont have default value
      var x = Toml.decode("address = '1.2.3.4'", TestObject)
      check x.address.isSome
      check x.address.get().value == "1.2.3.4"
      {.warning[UnsafeDefault]:on.}

type
  KV = object
    key: string

suite "toml v1.0.0 spec":
  test "raw tab in single/multi line basic string":
    var x = Toml.decode("key = \"\t\"", KV)
    check x.key == "\t"

    x = Toml.decode("key = \"\"\"\t\"\"\"", KV)
    check x.key == "\t"

  test "invalid chars in comment":
    expect TomlError:
      discard Toml.decode("# com\x00ment \nkey = \"hello\"", KV)

    expect TomlError:
      discard Toml.decode("# com\x08ment \nkey = \"hello\"", KV)

    expect TomlError:
      discard Toml.decode("# com\x0Ament \nkey = \"hello\"", KV)

    expect TomlError:
      discard Toml.decode("# com\x1Fment \nkey = \"hello\"", KV)

    expect TomlError:
      discard Toml.decode("# com\x7Fment \nkey = \"hello\"", KV)

  test "trailing commas not allowed in inline table":
    expect TomlError:
      discard Toml.decode("key = {subkey: \"val\", } ", TomlValueRef)

  test "leading zeroes in exponent parts of float":
    check Toml.decode("key = 10.0e001", float64, "key") == 10.0e1'f64

  test "no surrogate pairs in string":
    expect TomlError:
      discard Toml.decode("key = \"\\uD8000\"", KV)

    expect TomlError:
      discard Toml.decode("key = \"\\uDFFF\"", KV)

  test "EOF is allowed after key value pair":
    var x = Toml.decode("key = \"hello\"", KV)
    check x.key == "hello"

  test "various styles of writing keys":
    var x = Toml.decode("fruit. color = \"yellow\"    # same as fruit.color", TomlValueRef)
    check x["fruit"]["color", string] == "yellow"

    x = Toml.decode("fruit . color = \"yellow\"    # same as fruit.color", TomlValueRef)
    check x["fruit"]["color", string] == "yellow"

  test "line-ending backslashes must be unescaped in multi-line strings.":
    const text = "equivalent text"
    check Toml.decode("key = \"\"\"equivalent \\\n    text\"\"\"", string, "key") == text

  test "comments and newlines are allowed before commas in arrays":
    let x = Toml.decode("key = [123 # comments\n, 345]", array[2, int], "key")
    check x[1] == 345

  test "ignored indentation":
    check Toml.decode("  key = 123", int, "key") == 123
    check Toml.decode("  [table]\n  key = 123", int, "table.key") == 123
    check Toml.decode("  key = [  123,   123 ]", array[2, int], "key") == [123, 123]
