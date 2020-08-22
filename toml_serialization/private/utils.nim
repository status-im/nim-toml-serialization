# toml-serialization
# Copyright (c) 2020 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  strutils,
  ../types


type
  CodecState* = enum
    TopLevel
    InsideRecord
    ExpectValue

func cmpNimIdent(a, b: string): bool =
  if a.len == 0 and b.len == 0:
    return true
  if a.len == 0 or b.len == 0:
    return false
  if a[0] != b[0]:
    return false

  var i = 1
  var j = 1
  while true:
    while i < a.len and a[i] == '_': inc i
    while j < b.len and b[j] == '_': inc j
    var aa = if i < a.len: toLowerAscii(a[i]) else: '\0'
    var bb = if j < b.len: toLowerAscii(b[j]) else: '\0'
    let res = ord(aa) - ord(bb)
    if res != 0: return false
    # the characters are identical:
    if i >= a.len:
      # both cursors at the end:
      if j >= b.len: return true
      # not yet at the end of 'b':
      return false
    elif j >= b.len:
      return true
    inc i
    inc j

func compare*(a, b: openArray[string], tomlCase: TomlCase): bool =
  case tomlCase
  of TomlCaseSensitive:
    result = a == b
  of TomlCaseInsensitive:
    if a.len != b.len: return false
    for i, x in a:
      if cmpIgnoreCase(x, b[i]) != 0:
        return false
    result = true
  of TomlCaseNim:
    if a.len != b.len: return false
    for i, x in a:
      if not cmpNimIdent(x, b[i]):
        return false
    result = true
