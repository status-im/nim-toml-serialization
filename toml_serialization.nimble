mode = ScriptMode.Verbose

packageName   = "toml_serialization"
version       = "0.2.6"
author        = "Status Research & Development GmbH"
description   = "Flexible TOML serialization [not] relying on run-time type information"
license       = "Apache License 2.0"
skipDirs      = @["tests", "assets"]

requires "nim >= 1.6.0",
         "serialization",
         "stew"

let nimc = getEnv("NIMC", "nim") # Which nim compiler to use
let lang = getEnv("NIMLANG", "c") # Which backend (c/cpp/js)
let flags = getEnv("NIMFLAGS", "") # Extra flags for the compiler
let verbose = getEnv("V", "") notin ["", "0"]

let cfg =
  " --styleCheck:usages --styleCheck:error" &
  (if verbose: "" else: " --verbosity:0 --hints:off") &
  " --outdir:build --nimcache:build/nimcache -f"

proc build(args, path: string) =
  exec nimc & " " & lang & " " & cfg & " " & flags & " " & args & " " & path

proc run(args, path: string) =
  build args & " -r", path
  if (NimMajor, NimMinor) > (1, 6):
    build args & " --mm:refc -r", path

task test, "Run all tests":
  for threads in ["--threads:off", "--threads:on"]:
    run threads & " -d:release ", "tests/test_all"
