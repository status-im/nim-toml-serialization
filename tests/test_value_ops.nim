# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest2, os, options, tables,
  ../toml_serialization,
  ../toml_serialization/value_ops

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
