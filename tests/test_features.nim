# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest2, os, strutils, tables,
  ../toml_serialization,
  ../toml_serialization/lexer,
  stint

type
  Owner = object
    name: string
    dob: TomlDateTime
    cap: float64
    dobToml: string

  FakeOwner = tuple[name: string, cap: float64]

  Server = object
    ip: string
    port: int
    name: string

  Encoding = object
    name: string
    charset: string

  HoldDateTime = object
    dt: TomlDateTime

  HoldString = object
    name: string
    multiLine: bool
    literal: bool

  DateString = object
    date: string

  HoldFloat = object
    data: string
    sign: Sign

  HoldInt = object
    data: int

  Features = enum
    ParseInt
    ParseEnum
    ParseFloat
    ParseString

  HoldEnum = object
    data: Features

  HoldValue = object
    data: TomlValueRef

  HoldTable = object
    data: Table[string, int]

proc readValue*(r: var TomlReader, value: var UInt256) =
  var z: string
  let (sign, base) = r.parseNumber(z)

  if sign == Sign.Neg:
    raiseTomlErr(r.lex, errNegateUint)

  case base
  of base10: value = parse(z, UInt256, 10)
  of base16: value = parse(z, UInt256, 16)
  of base8:  value = parse(z, UInt256, 8)
  of base2:  value = parse(z, UInt256, 2)

proc readValue*(r: var TomlReader, value: var HoldDateTime) =
  value.dt = r.parseDateTime()

proc readValue*(r: var TomlReader, value: var HoldString) =
  (value.multiLine, value.literal) = r.parseString(value.name)

proc readValue*(r: var TomlReader, value: var DateString) =
  value.date = r.parseAsString

proc readValue*(r: var TomlReader, value: var HoldFloat) =
  value.sign = r.parseFloat(value.data)

proc readValue*(r: var TomlReader, value: var HoldInt) =
  value.data = r.parseInt(type value.data)

proc readValue*(r: var TomlReader, value: var HoldEnum) =
  value.data = r.parseEnum(type value.data, allowNumericRepr = true)

proc readValue*(r: var TomlReader, value: var HoldValue) =
  value.data = r.parseValue()

proc readValue*(r: var TomlReader, value: var Table) =
  parseTable(r, key):
    value[key] = r.parseInt(int)

type
  HoldArray = object
    data: array[3, int]

  HoldSeq = object
    data: seq[int]

  WelderFlag = enum
    TIG
    MIG
    MMA

  Welder = object
    flags: set[WelderFlag]

proc readValue*(r: var TomlReader, value: var HoldArray) =
  r.parseList(i):
    value.data[i] = r.parseInt(int)

proc readValue*(r: var TomlReader, value: var HoldSeq) =
  r.parseList:
    let lastPos = value.data.len
    value.data.setLen(lastPos + 1)
    readValue(r, value.data[lastPos])

proc readValue*(r: var TomlReader, value: var Welder) =
  r.parseList:
    value.flags.incl r.parseEnum(WelderFlag, allowNumericRepr = true)

suite "features test suite":
  let rawToml = readFile("tests" / "tomls" / "example.toml")
  let rawCase = readFile("tests" / "tomls" / "case.toml")

  test "case sensitive":
    let server = Toml.decode(rawToml, string, "database.server")
    check server == "192.168.1.1"

    let owner = Toml.decode(rawToml, string, "owner.name")
    check owner == "Tom Preston-Werner"

    let serverName = Toml.decode(rawCase, string, "SeRveR.nAmE")
    check serverName == "Toml"

    expect TomlError:
      discard Toml.decode(rawToml, string, "Fruit")

  test "case insensitive":
    let fruit = Toml.decode(rawCase, string, "fruit.name", TomlCaseInsensitive)
    check fruit == "banana"

    let animal = Toml.decode(rawCase, string, "animal.name", TomlCaseInsensitive)
    check animal == "Elephant"

  test "case nim style":
    let fruit = Toml.decode(rawCase, string, "Fruit.Name", TomlCaseNim)
    check fruit == "banana"

    let vehicle = Toml.decode(rawCase, string, "Vehicle.Name", TomlCaseNim)
    check vehicle == "hovercraft"

    let server = Toml.decode(rawCase, string, "Server.name", TomlCaseNim)
    check server == "Toml"

  test "allowUnknownFields":
    expect TomlError:
      discard Toml.decode(rawToml, Owner, "owner")

    var owner = Toml.decode(rawToml, Owner, "owner", TomlCaseInsensitive, {TomlUnknownFields})
    check owner.name == "Tom Preston-Werner"

    var fakeOwner = Toml.decode(rawToml, FakeOwner, "owner", TomlCaseInsensitive, {TomlUnknownFields})
    check fakeOwner.name == "Tom Preston-Werner"

    type
      NoName = object
        age: int

    let toml = readFile("tests" / "tomls" / "nested_object.toml")
    let y = Toml.decode(toml, NoName, "someone.noname", {TomlUnknownFields})
    check y.age == 30

  test "newline in inline table":
    let toml = readFile("tests" / "tomls" / "inline-table-newline.toml")

    expect TomlError:
      discard Toml.decode(toml, Server, "server")

    var server = Toml.decode(toml, Server, "server", TomlCaseInsensitive, {TomlInlineTableNewline})
    check server.port == 8005

    var z = Toml.decode(toml, Encoding, "encoding", TomlCaseInsensitive, {TomlInlineTableNewline})
    check z.name == "TOML"

    var w = Toml.decode(toml, TomlValueRef, {TomlInlineTableNewline})
    check w.tableVal["server"].kind == TomlKind.InlineTable

    expect TomlError:
      discard Toml.decode(toml, TomlValueRef)

  test "bignum":
    var z = Toml.decode("bignum = 1234567890_1234567890", UInt256, "bignum")
    check $z == "12345678901234567890"

  test "helper functions":
    var u = Toml.decode("val = 1970-08-08 07:10:11", HoldDateTime, "val")
    check u.dt.date.isSome

    var v = Toml.decode("val = \'Toml Literal\'", HoldString, "val")
    check:
      v.name == "Toml Literal"
      v.multiLine == false
      v.literal == true

    var w = Toml.decode("val = 1970-08-08 07:10:11", DateString, "val")
    check w.date == "1970-08-08 07:10:11"

    var z = Toml.decode("val = -123.123", HoldFloat, "val")
    check z.sign == Sign.Neg
    check z.data == "-123.123"

    var x = Toml.decode("val = 768", HoldInt, "val")
    check x.data == 768

    var y = Toml.decode("val = 1", HoldEnum, "val")
    check y.data == ParseEnum

    y = Toml.decode("val = 'ParseInt'", HoldEnum, "val")
    check y.data == ParseInt

    y = Toml.decode("val = \"ParseFloat\"", HoldEnum, "val")
    check y.data == ParseFloat

    expect TomlError:
      discard Toml.decode("val = '''ParseString'''", HoldEnum, "val")

    expect TomlError:
      discard Toml.decode("val = \"\"\"ParseString\"\"\"", HoldEnum, "val")

    var p = Toml.decode("val = 1", HoldValue, "val")
    check p.data == TomlValueRef(kind: TomlKind.Int, intVal: 1)

  test "hex escape sequence":
    var x = Toml.decode("val = \"H\\x45X\"", string, "val", TomlCaseSensitive, {TomlHexEscape})
    check x == "HEX"

    expect TomlError:
      discard Toml.decode("val = \"H\\x45X\"", string, "val")

    var z = Toml.decode("val = \"H\\x45X\" \n name = \"skip hex\"",
      string, "name", TomlCaseSensitive, {TomlHexEscape})
    check z == "skip hex"

    expect TomlError:
      discard Toml.decode("val = \"H\\x45X\" \n name = \"skip hex\"", string, "name")

  test "api test":
    var x = Toml.decode("val = \"H\\x45X\"", string, "val", {TomlHexEscape})
    check x == "HEX"

    var w = Toml.decode("Val = \"HEX\"", string, "Val", TomlCaseSensitive)
    check w == "HEX"

    var u = Toml.decode("val = 123456", int, "val")
    check u == 123456

    let file = "tests" / "tomls" / "example.toml"
    var z = Toml.loadFile(file, bool, "database.enabled")
    check z == true

    let toml = "tests" / "tomls" / "case.toml"
    var v = Toml.loadFile(toml, string, "animal.name", TomlCaseInsensitive)
    check v == "Elephant"

  test "unsupported keyed mode":
    let table = "tests" / "tomls" / "inline-table-newline.toml"
    expect TomlError:
      discard Toml.loadFile(table, int, "server.port", {TomlInlineTableNewline})

  test "date time reader":
    var x = Toml.decode("x = 12:13:14", TomlTime, "x")
    check x.hour == 12
    check x.minute == 13
    check x.second == 14

    var y = Toml.decode("x = 2020-08-16", TomlDate, "x")
    check y.year == 2020
    check y.month == 8
    check y.day == 16

  test "TomlHourMinute flags":
    expect TomlError:
      discard Toml.decode("x = 12:13", TomlTime, "x")

    var x = Toml.decode("x = 12:13", TomlTime, "x", {TomlHourMinute})
    check x.hour == 12
    check x.minute == 13
    check x.second == 0

suite "helper parsers":
  test "parseTable":
    var q = Toml.decode("p = 1\nq = 2\nr = 3", Table[string, int])
    check:
      q["p"] == 1
      q["q"] == 2
      q["r"] == 3

    var r = Toml.decode("[data]\np = 1\nq = 2\nr = 3", HoldTable)
    check:
      r.data["p"] == 1
      r.data["q"] == 2
      r.data["r"] == 3

    var s = Toml.decode("data = {p = 1, q = 2, r = 3}", HoldTable)
    check:
      s.data["p"] == 1
      s.data["q"] == 2
      s.data["r"] == 3

  test "parseList":
    let x = Toml.decode("x = [1, 2, 3]", HoldArray, "x")
    check x.data == [1, 2, 3]

    let y = Toml.decode("x = [1, 2, 3]", HoldSeq, "x")
    check y.data == @[1, 2, 3]

    let z = Toml.decode("x = ['MMA', 'MIG']", Welder, "x")
    check:
      MMA in z.flags
      MIG in z.flags

type
  Fruits = enum
    Apple
    Banana
    Orange

  FruitBasket = object
    fruit1: Fruits
    fruit2: Fruits
    fruit3: Fruits

proc readValue*(r: var TomlReader, value: var Fruits) =
  value = r.parseEnum(type value, allowNumericRepr = true)

proc writeValue*(w: var TomlWriter, val: Fruits) =
  w.writeValue $val

suite "enums examples":
  test "read enums":
    var x = Toml.loadFile("tests" / "tomls" / "fruits.toml", FruitBasket)
    check x.fruit1 == Apple
    check x.fruit2 == Banana
    check x.fruit3 == Orange

  test "write enums":
    let z = FruitBasket(fruit1: Apple, fruit2: Banana, fruit3: Orange)
    let res = Toml.encode(z)
    let want = "fruit1 = \"Apple\"\nfruit2 = \"Banana\"\nfruit3 = \"Orange\"\n"
    check res == want
