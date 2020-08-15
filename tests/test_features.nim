# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest, os, strutils, tables,
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

proc readValue*(r: var TomlReader, value: var Uint256) =
  var z: string
  let (sign, base) = r.parseNumber(z)

  if sign == Sign.Neg:
    raiseTomlErr(r.lex, errNegateUint)

  case base
  of base10: value = parse(z, Uint256, 10)
  of base16: value = parse(z, Uint256, 16)
  of base8:  value = parse(z, Uint256, 8)
  of base2:  value = parse(z, Uint256, 2)

proc readValue*(r: var TomlReader, value: var HoldDateTime) =
  value.dt = r.parseDateTime()

proc readValue*(r: var TomlReader, value: var HoldString) =
  (value.multiLine, value.literal) = r.parseString(value.name)

proc readValue*(r: var TomlReader, value: var DateString) =
  value.date = r.parseAsString

proc readValue*(r: var TomlReader, value: var HoldFloat) =
  value.sign = r.parseFloat(value.data)

proc main() =
  suite "features test suite":
    let rawToml = readFile("tests" / "tomls" / "example.toml")
    let rawCase = readFile("tests" / "tomls" / "case.toml")

    test "case sensitive":
      let server = Toml.decode(rawToml, string, "database.server")
      check server == "192.168.1.1"

      var owner = Toml.decode(rawToml, string, "owner.name")
      check owner == "Tom Preston-Werner"

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

    test "allowUnknownFields":
      expect TomlError:
        discard Toml.decode(rawToml, Owner, "owner")

      var owner = Toml.decode(rawToml, Owner, "owner", TomlCaseInsensitive, allowUnknownFields = true)
      check owner.name == "Tom Preston-Werner"

      var fakeOwner = Toml.decode(rawToml, FakeOwner, "owner", TomlCaseInsensitive, allowUnknownFields = true)
      check fakeOwner.name == "Tom Preston-Werner"

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
      var z = Toml.decode("bignum = 1234567890_1234567890", Uint256, "bignum")
      check $z == "12345678901234567890"

    test "builtins":
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

main()
