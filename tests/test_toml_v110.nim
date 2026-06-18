# toml-serialization
# Copyright (c) 2026 Status Research & Development GmbH
# Licensed under either of
#  * Apache License, version 2.0, ([LICENSE-APACHE](LICENSE-APACHE))
#  * MIT license ([LICENSE-MIT](LICENSE-MIT))
# at your option.
# This file may not be copied, modified, or distributed except according to
# those terms.

import
  unittest2,
  ../toml_serialization

#    TomlUnknownFields # allow unknow fields
#    TomlStrictComma   # array/inline table elements must be separated by a comma
type
  Apple = object
    color: string

  Fruit = object
    apple: Apple
    banana: string
    cocoa: TomlTime

  UnknownField = object
    apple: int

const
  tomlText = """
apple = {
  color = "red",
}
banana = "\x61\x62\x63"
cocoa = 15:16
"""

  unknownFields1 = """
banana = 2
apple = 1
"""

  unknownFields2 = """
apple = 1
banana = 2
"""

suite "Test if TOML v1.1.0 enabled by default":
  test "TomlInlineTableNewline, TomlInlineTableTrailingComma, TomlHexEscape, TomlHourMinute":
    let x = Toml.decode(tomlText, Fruit)
    check:
      x.apple.color == "red"
      x.banana == "abc"
      x.cocoa.hour == 15
      x.cocoa.minute == 16

  test "TomlStrictComma":
    expect TomlError:
      let x = Toml.decode("apple = [\"a\" \"b\" \"c\"]\n", seq[string], key = "apple")

  test "TomlUnknownField not enabled":
    expect TomlError:
      let x = Toml.decode(unknownFields1, UnknownField)

    # no exception if the unknown fields come after
    # known fields due to internal optimization
    let z = Toml.decode(unknownFields2, UnknownField)
    check z.apple == 1
