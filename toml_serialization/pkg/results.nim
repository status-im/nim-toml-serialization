# toml-serialization
# Copyright (c) 2019-2025 Status Research & Development GmbH
# Licensed under either of
#  * Apache License, version 2.0, ([LICENSE-APACHE](LICENSE-APACHE))
#  * MIT license ([LICENSE-MIT](LICENSE-MIT))
# at your option.
# This file may not be copied, modified, or distributed except according to
# those terms.

{.push raises: [], gcsafe.}

import pkg/results, ../../toml_serialization/[reader, writer]

export results

template isOptionalInToml*[T](U: type Opt[T]): bool = true
template BaseType*[T](U: type Opt[T]): type = T

template shouldWriteField*[T](field: Opt[T]): bool =
  field.isOk

proc writeValue*[T](w: var TomlWriter, value: Opt[T]) {.raises: [IOError].} =
  mixin writeValue

  if value.isOk:
    w.writeValue(value.get)

proc readValue*[T](
    r: var TomlReader, value: var Opt[T]
) {.raises: [IOError, SerializationError].} =
  mixin readValue

  value.ok r.readValue(T)

proc readValue*[T](
  r: var TomlReader, value: var Opt[T], numRead: int
) {.gcsafe, raises: [SerializationError, IOError].} =
  value.ok()
  readValue(r, value.get, numRead)
