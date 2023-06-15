# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest2, os, options, tables,
  ../toml_serialization,
  ../toml_serialization/private/utils

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

  BelowObject = object
    name: string
    color: int

  SubObject = object
    below: BelowObject

  TopObject = object
    sub: SubObject

template runTest(x: untyped, flags: TomlFlags = {}) =
  type T = type x
  var toml = Toml.encode(x, flags)
  var z =  Toml.decode(toml, T, flags)
  check x == z

template testToHex(x: BiggestInt, len: Positive, expectedOutput: string) =
  var s = memoryOutput()
  toHex(s, x, len)
  let output = s.getOutput string
  check output == expectedOutput

template testWriteInt(x: BiggestInt, len: Positive, expectedOutput: string) =
  var s = memoryOutput()
  writeInt(s, x, len)
  let output = s.getOutput string
  check output == expectedOutput

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
      {.warning[UnsafeDefault]:off.}
      runTest(x)
      {.warning[UnsafeDefault]:on.}

    test "nested object":
      var y = NestedObject(child: ChildObject(name: "Toml"))
      runTest(y)

    test "inline table":
      let x = TopObject(
        sub: SubObject(
          below: BelowObject( name: "below", color: 11)
        )
      )
      runTest(x)

      runTest(x, flags = {TomlInlineTableNewline})

    test "TomlTime":
      type
        TopTime = object
          time: TomlTime

      var z = TopTime(time: TomlTime(hour: 11, minute: 12, second: 0, subsecond: 11))
      runTest(z)
      runTest(z, flags = {TomlHourMinute})

      var w = TopTime(time: TomlTime(hour: 11, minute: 12, second: 0, subsecond: 0))
      runTest(w)
      runTest(w, flags = {TomlHourMinute})

    test "TomlHexEscape":
      type
        StringObject = object
          text: string

      var x = StringObject(text: "\x01\x02")
      runTest(x)
      runTest(x, flags = {TomlHexEscape})

    test "toHex":
      testToHex(1, 1, "1")
      testToHex(1, 2, "01")
      testToHex(10, 2, "0A")
      testToHex(11, 4, "000B")
      testToHex(1234, 4, "04D2")
      testToHex(0x1234, 3, "234")

    test "writeInt":
      testWriteInt(0, 1, "0")
      testWriteInt(1, 1, "1")
      testWriteInt(1, 2, "01")
      testWriteInt(10, 2, "10")
      testWriteInt(10, 1, "0")
      testWriteInt(11, 2, "11")
      testWriteInt(11, 3, "011")

proc testTableArray() =
  suite "table array encoder":
    test "basic table array encoder":
      type
        Disc = object
          sector: int
          cylinder: int

        SeqList = object
          disc: seq[Disc]

        ArrayList = object
          disc: array[3, Disc]

      var x = SeqList()
      x.disc.add Disc(sector: 128, cylinder: 16)
      x.disc.add Disc(sector: 256, cylinder: 8)
      x.disc.add Disc(sector: 512, cylinder: 32)
      runTest(x)

      var y = ArrayList()
      y.disc[0] = Disc(sector: 128, cylinder: 16)
      y.disc[1] = Disc(sector: 256, cylinder: 8)
      y.disc[2] = Disc(sector: 512, cylinder: 32)
      runTest(y)

proc testOptionalField() =
  suite "optional field encoder":
    test "encode optional field":
      type
        Vehicle = object
          bumper: Option[string]
          antennae: Option[int]

      let v = Vehicle(
        bumper: some("Chromium")
      )

      let w = Toml.encode(v)
      let u = Toml.decode(w, Vehicle)
      check u == v

proc testEnums() =
  suite "enum encoder":
    test "OrdinalEnum":
      type
        EnumTest = enum
          x0,
          x1,
          x2
        Wrapper = object
          v: EnumTest

      check:
        Toml.encode(Wrapper(v: x0)) == "v = \"x0\"\n"
        Toml.encode(Wrapper(v: x1)) == "v = \"x1\"\n"
        Toml.encode(Wrapper(v: x2)) == "v = \"x2\"\n"

    test "HoleyEnum":
      type
        EnumTest = enum
          y1 = 1,
          y3 = 3,
          y4,
          y6 = 6
        Wrapper = object
          v: EnumTest

      check:
        Toml.encode(Wrapper(v: y1)) == "v = \"y1\"\n"
        Toml.encode(Wrapper(v: y3)) == "v = \"y3\"\n"
        Toml.encode(Wrapper(v: y4)) == "v = \"y4\"\n"
        Toml.encode(Wrapper(v: y6)) == "v = \"y6\"\n"

    test "StringEnum":
      type
        EnumTest = enum
          z1 = "aaa",
          z2 = "bbb",
          z3 = "ccc"
        Wrapper = object
          v: EnumTest

      check:
        Toml.encode(Wrapper(v: z1)) == "v = \"aaa\"\n"
        Toml.encode(Wrapper(v: z2)) == "v = \"bbb\"\n"
        Toml.encode(Wrapper(v: z3)) == "v = \"ccc\"\n"

main()
testTableArray()
testOptionalField()
testEnums()
