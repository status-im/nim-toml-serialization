# toml_serialization
# Copyright (c) 2018-2025 Status Research & Development GmbH
# Licensed under either of
#  * Apache License, version 2.0, ([LICENSE-APACHE](LICENSE-APACHE))
#  * MIT license ([LICENSE-MIT](LICENSE-MIT))
# at your option.
# This file may not be copied, modified, or distributed except according to
# those terms.

{.push raises: [], gcsafe.}

import ../../toml_serialization, ../../toml_serialization/lexer, std/uri
export uri

proc writeValue*(w: var TomlWriter, value: Uri) {.raises: [IOError].} =
  w.writeValue($value)

proc readValue*(
    r: var TomlReader, value: var Uri
) {.raises: [IOError, SerializationError].} =
  let s = r.readValue(string)
  try:
    value = parseUri(s)
  except ValueError as exc:
    r.lex.raiseUnexpectedValue(exc.msg)

{.pop.}
