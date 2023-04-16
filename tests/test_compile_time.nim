# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  unittest2,
  ../toml_serialization

type
  Fruits = object
    fruit1: string
    fruit2: int
    fruit3: string

proc toBlob(x: string): seq[byte] =
  result = newSeq[byte](x.len)
  for i, c in x: result[i] = c.byte

const
  fruitFile = "tomls/fruits.toml"
  toml = staticRead fruitFile
  xx = Toml.loadFile("tests/" & fruitFile, Fruits)
  yy = Toml.decode(toml, Fruits)
  zz = Toml.decode(toml.toBlob, Fruits)
  kk = Toml.decode(toml, string, "fruit3")

suite "compile time decoder":
  test "compile time loadFile":
    check xx.fruit1 == "Apple"
    check xx.fruit2 == 1
    check xx.fruit3 == "Orange"

  test "compile time decode string":
    check yy.fruit1 == "Apple"
    check yy.fruit2 == 1
    check yy.fruit3 == "Orange"

  test "compile time decode blob":
    check zz.fruit1 == "Apple"
    check zz.fruit2 == 1
    check zz.fruit3 == "Orange"

  test "compile time keyed":
    check kk == "Orange"