import
  unittest, os,
  ../toml_serialization

proc runValidTest(inputFolder: string) =
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

proc runInvalidTest(inputFolder: string) =
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

proc main() =
  runValidTest("iarna")
  runValidTest("burntsushi")

  runInvalidTest("iarna")
  runInvalidTest("burntsushi")

main()
