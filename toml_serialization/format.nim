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
  serialization/formats,
  ./desc,
  ./types,
  ./decoder

export
  formats,
  TomlSpecial,
  TomlNotTopLevel

template flavorRuntimeFlags*(_: type Toml): set[TomlFlag] = {}
template flavorUsesAutomaticObjectSerialization*(_: type Toml): bool = true
template flavorUsesAutomaticPrimitivesSerialization*(_: type Toml): bool = true
template flavorAutoSerializationRead*(_: type Toml, _: distinct type): bool = true
template flavorAutoSerializationWrite*(_: type Toml, _: distinct type): bool = true

template createTomlFlavor*(FlavorName: untyped,
                           mimeTypeValue = "application/toml",
                           automaticObjectSerialization = false,
                           automaticPrimitivesSerialization = true,
                           runtimeFlags: set[TomlFlag] = {}) {.dirty.} =

  createFlavor(Toml, FlavorName, mimeTypeValue)
  template flavorRuntimeFlags*(_: distinct type FlavorName): set[TomlFlag] = runtimeFlags
  template flavorUsesAutomaticObjectSerialization*(_: distinct type FlavorName): bool = automaticObjectSerialization
  template flavorUsesAutomaticPrimitivesSerialization*(_: distinct type FlavorName): bool = automaticPrimitivesSerialization
  template flavorAutoSerializationRead*(_: distinct type FlavorName, _: distinct type): bool = automaticPrimitivesSerialization
  template flavorAutoSerializationWrite*(_: distinct type FlavorName, _: distinct type): bool = automaticPrimitivesSerialization

template setAutoSerializationRead*(Flavor: type SerializationFormat, TargetType: distinct type) =
  mixin flavorUsesAutomaticPrimitivesSerialization
  when flavorUsesAutomaticPrimitivesSerialization(Flavor):
    const
      flavorName = typetraits.name(Flavor)
      typeName = typetraits.name(TargetType)
    {.error: flavorName & ": please set automaticPrimitivesSerialization to false".}
  template flavorAutoSerializationRead*(_: type Flavor, _: distinct type TargetType): bool = true

template setAutoSerializationWrite*(Flavor: type SerializationFormat, TargetType: distinct type) =
  mixin flavorUsesAutomaticPrimitivesSerialization
  when flavorUsesAutomaticPrimitivesSerialization(Flavor):
    const
      flavorName = typetraits.name(Flavor)
      typeName = typetraits.name(TargetType)
    {.error: flavorName & ": please set automaticPrimitivesSerialization to false".}
  template flavorAutoSerializationWrite*(_: type Flavor, _: distinct type TargetType): bool = true

template setAutoSerialization*(Flavor: type SerializationFormat, TargetType: distinct type) =
  setAutoSerializationRead(Flavor, TargetType)
  setAutoSerializationWrite(Flavor, TargetType)
