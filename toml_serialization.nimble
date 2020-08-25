mode = ScriptMode.Verbose

packageName   = "toml_serialization"
version       = "0.1.0"
author        = "Status Research & Development GmbH"
description   = "Flexible TOML serialization [not] relying on run-time type information"
license       = "Apache License 2.0"
skipDirs      = @["tests", "assets"]

requires "nim >= 1.1.2",
         "serialization",
         "stew"

task test, "Run all tests":
  exec "nim c -r --threads:off -d:release tests/test_all"
  exec "nim c -r --threads:on -d:release tests/test_all"
