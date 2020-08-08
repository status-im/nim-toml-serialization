# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest, os,
  ../toml_serialization,
  ../toml_serialization/lexer

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

proc testDecoder() =
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
        ex.configCap == JsonCap
        ex.cc == TomlCap
        ex.fruit == Banana

    test "spec example":
      discard Toml.loadFile("tests" / "tomls" / "example.toml", TomlValueRef)

    test "nested object":
      var x = Toml.loadFile("tests" / "tomls" / "nested_object.toml", NestedObject)
      check:
        x.child.name == "TOML"
        x.child.child.name == "CHILD"
        x.child.child.age == 10

testDecoder()
