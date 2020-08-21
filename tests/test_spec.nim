import
  unittest, os, options, tables,
  ../toml_serialization

proc validInputTest(inputFolder: string) =
  suite inputFolder & " valid":
    var failed = 0
    for fileName in walkDirRec("tests" / "tomls" / inputFolder / "valid"):
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

proc invalidInputTest(inputFolder: string) =
  suite inputFolder & " invalid":
    var failed = 0
    for fileName in walkDirRec("tests" / "tomls" / inputFolder / "invalid"):
      test fileName:
        try:
          discard Toml.loadFile(fileName, TomlValueRef)
          inc failed
          check false
        except TomlError:
          check true
    if failed > 0:
      debugEcho "failed: ", failed

template roundTrip(fileName: string, params: varargs[untyped]): untyped =
  let
    toml = Toml.loadFile(fileName, TomlValueRef, params)
    tomlBytes = Toml.encode(toml, params)
    tomlVal = Toml.decode(tomlBytes, TomlValueRef, params)

  toml == tomlVal

proc roundTripTest(inputFolder: string) =
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

proc main() =
  validInputTest("iarna")
  validInputTest("burntsushi")

  invalidInputTest("iarna")
  invalidInputTest("burntsushi")

  roundTripTest("iarna")
  roundTripTest("burntsushi")

  suite "toml-serialization test suite":
    test "case.toml":
      check roundTrip("tests" / "tomls" / "case.toml")

    test "example.toml":
      check roundTrip("tests" / "tomls" / "example.toml")

    test "nested_object.toml":
      check roundTrip("tests" / "tomls" / "nested_object.toml")

    test "spec.toml":
      check roundTrip("tests" / "tomls" / "spec.toml")

    test "inline-table-newline.toml":
      check roundTrip("tests" / "tomls" / "inline-table-newline.toml", flags = {TomlInlineTableNewline})

main()
