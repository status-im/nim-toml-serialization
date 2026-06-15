# toml-serialization
# Copyright (c) 2026 Status Research & Development GmbH
# Licensed under either of
#  * Apache License, version 2.0, ([LICENSE-APACHE](LICENSE-APACHE))
#  * MIT license ([LICENSE-MIT](LICENSE-MIT))
# at your option.
# This file may not be copied, modified, or distributed except according to
# those terms.

import
  serialization/[formats, object_serialization],
  ./desc,
  ./types

export formats

template flavorRuntimeFlags*(_: type Toml, T: type DefaultFlavor): set[TomlFlag] = {}
template flavorUsesAutomaticObjectSerialization*(_: type Toml, T: type DefaultFlavor): bool = true

template createTomlFlavor*(FlavorName: untyped,
                           mimeTypeValue = "application/toml",
                           automaticObjectSerialization = false,
                           runtimeFlags: set[TomlFlag] = {}) {.dirty.} =

  createFlavor(Toml, FlavorName, mimeTypeValue)

  template flavorRuntimeFlags*(_: type Toml, T: type FlavorName): set[TomlFlag] = runtimeFlags
  template flavorUsesAutomaticObjectSerialization*(_: type Toml, T: type FlavorName): bool = automaticObjectSerialization
