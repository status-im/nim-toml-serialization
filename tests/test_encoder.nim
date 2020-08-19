# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest, os, options, tables,
  ../toml_serialization

type
  Fruits = enum
    Apple
    Banana
    Papaya

  Example = object
    boolean: bool
    time: TomlTime
    date: TomlDate
    dateTime: TomlDateTime
    stringField: string
    intField: int
    uintField: uint
    int16Field: int16
    uint16Field: uint16
    enumField: Fruits
    floatField: float64
    arrayField: array[3, int]
    seqField: seq[int]
    rangeField: range[10..20]

  ChildObject = object
    name: string

  NestedObject = object
    child: ChildObject

template runTest(x: untyped) =
  type T = type x
  var toml = Toml.encode(x)
  var z =  Toml.decode(toml, T)
  check x == z

proc main() =
  let time = TomlTime(hour: 17, minute: 18, second: 19, subsecond: 20)
  let date = TomlDate(year: 1971, month: 11, day: 21)
  let zone = TomlTimeZone(positiveShift: false, hourShift: 7, minuteShift: 3)

  var x = Example(
    boolean: true,
    time: time,
    date: date,
    dateTime: TomlDateTime(date: some(date), time: some(time), zone: some(zone)),
    stringField: "TOML",
    intField: -127,
    uintField: 128,
    int16Field: -6543,
    uint16Field: 789,
    enumField: Banana,
    floatField: -123.9,
    arrayField: [7, 5, 3],
    seqField: @[9,4,1],
    rangeField: 11
  )

  suite "encoder test suite":
    test "basic data types":
      runTest(x)

    test "nested object":
      var y = NestedObject(child: ChildObject(name: "Toml"))
      runTest(y)

main()
