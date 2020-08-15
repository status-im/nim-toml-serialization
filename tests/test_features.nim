# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest, os, strutils,
  ../toml_serialization,
  ../toml_serialization/lexer

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

main()
