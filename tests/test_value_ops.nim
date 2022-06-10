# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest2, os, options, tables,
  ../toml_serialization,
  ../toml_serialization/[value_ops, types]

template roundTrip(fileName: string, params: varargs[untyped]): untyped =
  let
    toml = Toml.loadFile(fileName, TomlValueRef, params)
    copyVal = toml.copy

  toml == copyVal

template copyRoundTripTest(inputFolder: string) =
  suite inputFolder & " valid roundtrip":
    var failed = 0
    for fileName in walkDirRec("tests" / "tomls" / inputFolder / "valid"):
      test fileName:
        try:
          check roundTrip(fileName)
        except TomlError as e:
          debugEcho "ERROR: ", e.msg
          check false
          inc failed
    if failed > 0:
      debugEcho "failed: ", failed

copyRoundTripTest("iarna")
copyRoundTripTest("burntsushi")

suite "table comparison":
  let a = TomlValueRef(kind: TomlKind.Int, intVal: 1)
  let b = TomlValueRef(kind: TomlKind.Int, intVal: 2)
  let x = TomlValueRef(kind: TomlKind.Table, tableVal: TomlTableRef())
  let y = TomlValueRef(kind: TomlKind.Table, tableVal: TomlTableRef())
  let z = TomlValueRef(kind: TomlKind.Table, tableVal: TomlTableRef())

  x.tableVal["b"] = b
  x.tableVal["a"] = a

  y.tableVal["a"] = a
  y.tableVal["b"] = b

  z.tableVal["b"] = b
  z.tableVal["a"] = a

  test "different insertion order":
    when tomlOrderedTable:
      check x != y
    else:
      check x == y

  test "same insertion order":
    check x == z
