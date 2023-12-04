# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  std/[os, options, tables],
  unittest2,
  ../toml_serialization

template validInputTest(inputFolder: string) =
  suite inputFolder & " valid":
    var failed = 0
    for fileName in walkDirRec("tests/tomls/" & inputFolder & "/valid"):
      test fileName:
        try:
          discard Toml.loadFile(fileName, TomlValueRef)
          check true
        except TomlError as e:
          debugEcho "ERROR: ", e.msg
          check false
          inc failed
    if failed > 0:
      debugEcho "failed: ", failed

template invalidInputTest(inputFolder: string) =
  suite inputFolder & " invalid":
    var failed = 0
    for fileName in walkDirRec("tests/tomls/" & inputFolder & "/invalid"):
      test fileName:
        try:
          discard Toml.loadFile(fileName, TomlValueRef)
          inc failed
          check false
        except TomlError:
          check true
    if failed > 0:
      debugEcho "failed: ", failed

proc roundTrip(fileName: string, flags: set[TomlFlag] = {}): bool =
  let
    toml = Toml.loadFile(fileName, TomlValueRef, flags)
    tomlBytes = Toml.encode(toml, flags)
    tomlVal = Toml.decode(tomlBytes, TomlValueRef, flags)

  toml == tomlVal

template roundTripTest(inputFolder: string) =
  suite inputFolder & " valid roundtrip":
    var failed = 0
    for fileName in walkDirRec("tests/tomls/" & inputFolder & "/valid"):
      test fileName:
        try:
          check roundTrip(fileName)
        except TomlError as e:
          debugEcho "ERROR: ", e.msg
          check false
          inc failed
    if failed > 0:
      debugEcho "failed: ", failed

validInputTest("iarna")
validInputTest("burntsushi")

invalidInputTest("iarna")
invalidInputTest("burntsushi")

when not tomlOrderedTable:
  # TODO:
  # the encoder/writer still cannot produce
  # ordered result correctly

  roundTripTest("iarna")
  roundTripTest("burntsushi")

  suite "toml-serialization test suite":
    test "case.toml":
      check roundTrip("tests/tomls/case.toml")

    test "example.toml":
      check roundTrip("tests/tomls/example.toml")

    test "nested_object.toml":
      check roundTrip("tests/tomls/nested_object.toml")

    when not (defined(macosx) and defined(cpp)):
      # TODO: duplicate empty key exception raised when
      # macosx and cpp defined
      test "spec.toml":
        check roundTrip("tests/tomls/spec.toml")

    test "inline-table-newline.toml":
      check roundTrip("tests/tomls/inline-table-newline.toml", flags = {TomlInlineTableNewline})
