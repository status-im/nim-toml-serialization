mode = ScriptMode.Verbose

packageName   = "toml_serialization"
version       = "0.2.0"
author        = "Status Research & Development GmbH"
description   = "Flexible TOML serialization [not] relying on run-time type information"
license       = "Apache License 2.0"
skipDirs      = @["tests", "assets"]

requires "nim >= 1.1.2",
         "serialization",
         "stew"

### Helper functions
proc test(env, path: string) =
  # Compilation language is controlled by TEST_LANG
  var lang = "c"
  if existsEnv"TEST_LANG":
    lang = getEnv"TEST_LANG"

  when defined(macosx):
    # cpp backend on macosx have mysterious
    # bug
    if lang == "cpp":
      lang = "c"

  if not dirExists "build":
    mkDir "build"
  exec "nim " & lang & " " & env &
    " --outdir:build -r --hints:off --skipParentCfg " & path

task test, "Run all tests":
  exec "nim -v"
  test "--threads:off -d:release", "tests/test_all"
  test "--threads:on -d:release", "tests/test_all"
