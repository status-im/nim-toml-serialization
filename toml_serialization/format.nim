# toml-serialization
# Copyright (c) 2026 Status Research & Development GmbH
# Licensed under either of
#  * Apache License, version 2.0, ([LICENSE-APACHE](LICENSE-APACHE))
#  * MIT license ([LICENSE-MIT](LICENSE-MIT))
# at your option.
# This file may not be copied, modified, or distributed except according to
# those terms.

import
  std/typetraits,
  serialization/[formats, object_serialization],
  ./desc,
  ./types

export formats

template flavorRuntimeFlags*(_: type Toml, F: distinct type DefaultFlavor): set[TomlFlag] = {}
template flavorUsesAutomaticObjectSerialization*(_: type Toml, F: distinct type DefaultFlavor): bool = true
template flavorUsesAutomaticPrimitivesSerialization*(_: type Toml, F: distinct type DefaultFlavor): bool = true
template flavorAutoSerialization*(_: type Toml, F: distinct type DefaultFlavor, X: distinct type): bool = true

template createTomlFlavor*(FlavorName: untyped,
                           mimeTypeValue = "application/toml",
                           automaticObjectSerialization = false,
                           automaticPrimitivesSerialization = true,
                           runtimeFlags: set[TomlFlag] = {}) {.dirty.} =

  createFlavor(Toml, FlavorName, mimeTypeValue)

  template flavorRuntimeFlags*(_: type Toml, F: distinct type FlavorName): set[TomlFlag] = runtimeFlags
  template flavorUsesAutomaticObjectSerialization*(_: type Toml, F: distinct type FlavorName): bool = automaticObjectSerialization
  template flavorUsesAutomaticPrimitivesSerialization*(_: type Toml, F: distinct type FlavorName): bool = automaticPrimitivesSerialization
  template flavorAutoSerialization*(_: type Toml, F: distinct type FlavorName, X: distinct type): bool = automaticPrimitivesSerialization

template setAutoSerialization*(Flavor: type, X: distinct type) =
  mixin flavorUsesAutomaticPrimitivesSerialization
  when flavorUsesAutomaticPrimitivesSerialization(Toml, Flavor):
    const
      flavorName = typetraits.name(Flavor)
      typeName = typetraits.name(X)
    {.error: flavorName & ": please set automaticPrimitivesSerialization to false".}
  template flavorAutoSerialization*(_: type Toml, F: distinct type Flavor, _: distinct type X): bool = true
