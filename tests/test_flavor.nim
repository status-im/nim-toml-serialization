# toml-serialization
# Copyright (c) 2026 Status Research & Development GmbH
# Licensed under either of
#  * Apache License, version 2.0, ([LICENSE-APACHE](LICENSE-APACHE))
#  * MIT license ([LICENSE-MIT](LICENSE-MIT))
# at your option.
# This file may not be copied, modified, or distributed except according to
# those terms.

import
  std/[strutils, options, tables],
  unittest2,
  results,
  stew/byteutils,
  serialization,
  ../toml_serialization/pkg/results,
  ../toml_serialization

createTomlFlavor StringyToml

proc writeValue(w: var TomlWriter[StringyToml], value: seq[byte]) =
  w.writeValue('"' & toHex(value) & '"')

proc writeValue*(
    w: var TomlWriter[StringyToml], val: SomeInteger) {.raises: [IOError].} =
  writeValue(w, $val)

proc readValue*(r: var TomlReader[StringyToml], v: var SomeSignedInt) =
  try:
    v = type(v) parseBiggestInt readValue(r, string)
  except ValueError as err:
    r.raiseUnexpectedValue("A signed integer encoded as string " & err.msg)

proc readValue*(r: var TomlReader[StringyToml], v: var SomeUnsignedInt) =
  try:
    v = type(v) parseBiggestUInt readValue(r, string)
  except ValueError as err:
    r.raiseUnexpectedValue("An unsigned integer encoded as string " & err.msg)

type
  Container = object
    name: string
    x: int
    y: uint64
    list: seq[int64]

  OptionalFields = object
    one: Opt[string]
    two: Option[int]

  Banana = object
    color: string
    time: TomlTime

Container.useDefaultSerializationIn StringyToml

createTomlFlavor OptToml
OptionalFields.useDefaultSerializationIn OptToml

createTomlFlavor HexToml,
  runtimeFlags = {TomlHexEscape, TomlHourMinute}

Banana.useDefaultSerializationIn HexToml

suite "Test TOML Flavor":
  test "basic test":
    let c = Container(name: "c", x: -10, y: 20, list: @[1'i64, 2, 25])
    let encoded = StringyToml.encode(c)
    check encoded == "name = \"c\"\nx = \"-10\"\ny = \"20\"\nlist = [\"1\", \"2\", \"25\"]\n"

    let decoded = StringyToml.decode(encoded, Container)
    check decoded == Container(name: "c", x: -10, y: 20, list: @[1, 2, 25])

  test "optional fields":
    let a = OptionalFields(one: Opt.some("hello"))
    let b = OptionalFields(two: some(567))
    let c = OptionalFields(one: Opt.some("burn"), two: some(333))

    let aa = OptToml.encode(a)
    check aa == "one = \"hello\"\n"

    let bb = OptToml.encode(b)
    check bb == "two = 567\n"

    let cc = OptToml.encode(c)
    check cc == "one = \"burn\"\ntwo = 333\n"

  test "Flavor runtime flags":
    let b = HexToml.decode("color = \"\\x61\"\n time = 13:12\n", Banana)
    check:
      b.color == "\x61"
      b.color == "a"
      b.time.hour == 13
      b.time.minute == 12
