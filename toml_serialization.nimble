mode = ScriptMode.Verbose

packageName   = "toml_serialization"
version       = "0.3.0"
author        = "Status Research & Development GmbH"
description   = "Flexible TOML serialization [not] relying on run-time type information"
license       = "Apache License 2.0"
skipDirs      = @["tests", "assets"]

requires "nim >= 2.2.2",
         "faststreams >= 0.5.0",
         "serialization >= 0.5.3",
         "stew"

let nimc = getEnv("NIMC", "nim") # Which nim compiler to use
let lang = getEnv("NIMLANG", "c") # Which backend (c/cpp/js)
let flags = getEnv("NIMFLAGS", "") # Extra flags for the compiler
let verbose = getEnv("V", "") notin ["", "0"]
let platform = getEnv("PLATFORM", "")

let cfg =
  " --styleCheck:usages --styleCheck:error" &
  (if verbose: "" else: " --verbosity:0") &
  " --outdir:build --nimcache:build/nimcache -f"

proc build(args, path: string) =
  exec nimc & " " & lang & " " & cfg & " " & flags & " " & args & " " & path

proc run(args, path: string) =
  build args & " --mm:refc -r", path
  build args & " --mm:orc -r", path

task test, "Run all tests":
  for threads in ["--threads:off", "--threads:on"]:
    run threads & " -d:release ", "tests/test_all"

task test_asan, "Run all tests with ASAN":
  if platform != "x86":
    # https://clang.llvm.org/docs/AddressSanitizer.html
    putEnv("ASAN_OPTIONS", "detect_leaks=0:detect_stack_use_after_return=1")
    # https://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html
    putEnv("UBSAN_OPTIONS", "print_stacktrace=1")
    let asanArgs =
      " --mm:orc -d:useMalloc --cc:clang --debugger:native" &
      " --passC:-fsanitize=address,undefined" &
      " --passL:-fsanitize=address,undefined" &
      " --passC:-fno-sanitize-recover=undefined" &
      " --passC:-fno-sanitize-merge" &
      " --passC:-fno-omit-frame-pointer"
    for threads in ["--threads:off", "--threads:on"]:
      build threads & " -d:release" & asanArgs & " -r", "tests/test_all"
