# toml-serialization
# Copyright (c) 2026 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  std/tables,
  unittest2,
  stew/objects,
  ../toml_serialization

type
  Fat = object
    number: int
    boolean: bool
    strVal: string
    arrayVal: array[2, string]
    table: Table[string, string]

  Text = object
    one: Fat
    two: Fat
    three: Fat
    four: Fat
    five: Fat

const basic = """
one = 127
two = true
three = "hello toml"
four = ["toml", "test"]
five = {a="x", b="y", c="z"}
"""

proc readValue*(r: var TomlReader, value: var Fat) {.raises: [IOError, SerializationError].} =
  mixin readValue
  let tok = r.tokKind
  case tok
  of TomlTokKind.NumberOrDate:
    value.number = r.parseInt(int)
  of TomlTokKind.Bool:
    value.boolean = r.parseBool()
  of TomlTokKind.String:
    value.strVal = r.parseAsString()
  of TomlTokKind.Array:
    r.parseList(i):
      value.arrayVal[i] = r.parseAsString()
  of TomlTokKind.Table:
    r.parseTable(key):
      value.table[key] = r.parseAsString()

suite "Reader tokKind":
  test "basic types":
    let x = Toml.decode(basic, Text)
    check:
      x.one.number == 127
      x.two.boolean == true
      x.three.strVal == "hello toml"
      x.four.arrayVal == ["toml", "test"]
      x.five.table.len == 3
      x.five.table["a"] == "x"
      x.five.table["b"] == "y"
      x.five.table["c"] == "z"
