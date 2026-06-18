# toml-serialization
# Copyright (c) 2026 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest2,
  stew/objects,
  ../toml_serialization,
  ../toml_serialization/pkg/results

type
  OptionalFields = object
    one: Opt[string]
    two: Option[int]

  Meter = distinct int

  Simple = object
    x: int
    y: string
    distance: Meter

  HoldsResultOpt* = object
    o*: Opt[Simple]
    r*: Opt[Simple]

  ObjectWithOptionalFields = object
    a: Opt[int]
    b: Option[string]
    c: int

func `==`(lhs, rhs: Meter): bool =
  int(lhs) == int(rhs)

func simple(x: int, y: string, d: Meter): Simple =
  Simple(x: x, y: y, distance: d)

proc writeValue*(w: var TomlWriter, value: Meter) {.raises: [IOError].} =
  mixin writeValue
  w.writeValue(int value)

proc readValue*(r: var TomlReader, value: var Meter) {.raises: [IOError, SerializationError].} =
  mixin readValue
  value = Meter(r.readValue(int))

suite "Test results":
  test "Encode optional fields":
    let a = OptionalFields(one: Opt.some("hello"))
    let b = OptionalFields(two: some(567))
    let c = OptionalFields(one: Opt.some("burn"), two: some(333))

    let aa = Toml_v100.encode(a)
    check aa == "one = \"hello\"\n"

    let bb = Toml_v100.encode(b)
    check bb == "two = 567\n"

    let cc = Toml_v100.encode(c)
    check cc == "one = \"burn\"\ntwo = 333\n"

  test "Result Opt types":
    check:
      false == static(Toml.isFieldExpected Opt[Simple])
      2 == static(HoldsResultOpt.totalSerializedFields)
      0 == static(HoldsResultOpt.totalExpectedFields)

    let
      h1 = HoldsResultOpt(o: Opt[Simple].ok Simple(x: 1, y: "2", distance: Meter(3)))
      h2 = HoldsResultOpt(r: Opt[Simple].ok simple(1, "2", Meter(3)))

    let res = Toml_v100.encode h1
    check res == "o = {x = 1, y = \"2\", distance = 3}\n"
    let ser = Toml_v100.decode(res, HoldsResultOpt)
    check ser == h1

    let r2 = Toml_v100.encode h2
    check r2 == "r = {x = 1, y = \"2\", distance = 3}\n"
    let s2 = Toml_v100.decode(r2, HoldsResultOpt)
    check s2 == h2

  test "object with optional fields":
    let x = ObjectWithOptionalFields(
      a: Opt.some(123),
      b: some("nano"),
      c: 456,
    )

    let y = ObjectWithOptionalFields(
      a: Opt.none(int),
      b: none(string),
      c: 999,
    )

    let u = Toml.encode(x)
    check u == "a = 123\nb = \"nano\"\nc = 456\n"

    let v = Toml.encode(y)
    check v == "c = 999\n"
