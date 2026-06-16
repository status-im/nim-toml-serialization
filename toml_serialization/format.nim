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

template flavorRuntimeFlags*(_: type Toml, _: distinct type DefaultFlavor): set[TomlFlag] = {}
template flavorUsesAutomaticObjectSerialization*(_: type Toml, _: distinct type DefaultFlavor): bool = true
template flavorUsesAutomaticPrimitivesSerialization*(_: type Toml, _: distinct type DefaultFlavor): bool = true
template flavorAutoSerializationRead*(_: type Toml, _: distinct type DefaultFlavor, _: distinct type): bool = true
template flavorAutoSerializationWrite*(_: type Toml, _: distinct type DefaultFlavor, _: distinct type): bool = true

template createTomlFlavor*(FlavorName: untyped,
                           mimeTypeValue = "application/toml",
                           automaticObjectSerialization = false,
                           automaticPrimitivesSerialization = true,
                           runtimeFlags: set[TomlFlag] = {}) {.dirty.} =

  createFlavor(Toml, FlavorName, mimeTypeValue)

  template flavorRuntimeFlags*(_: type Toml, _: distinct type FlavorName): set[TomlFlag] = runtimeFlags
  template flavorUsesAutomaticObjectSerialization*(_: type Toml, _: distinct type FlavorName): bool = automaticObjectSerialization
  template flavorUsesAutomaticPrimitivesSerialization*(_: type Toml, _: distinct type FlavorName): bool = automaticPrimitivesSerialization
  template flavorAutoSerializationRead*(_: type Toml, _: distinct type FlavorName, _: distinct type): bool = automaticPrimitivesSerialization
  template flavorAutoSerializationWrite*(_: type Toml, _: distinct type FlavorName, _: distinct type): bool = automaticPrimitivesSerialization
  Toml.setDecoder(FlavorName)

template setAutoSerializationRead*(Flavor: type SerializationFormat, TargetType: distinct type) =
  mixin flavorUsesAutomaticPrimitivesSerialization
  when flavorUsesAutomaticPrimitivesSerialization(Toml, Flavor):
    const
      flavorName = typetraits.name(Flavor)
      typeName = typetraits.name(TargetType)
    {.error: flavorName & ": please set automaticPrimitivesSerialization to false".}
  template flavorAutoSerializationRead*(_: type Toml, _: type Flavor, _: distinct type TargetType): bool = true

template setAutoSerializationWrite*(Flavor: type SerializationFormat, TargetType: distinct type) =
  mixin flavorUsesAutomaticPrimitivesSerialization
  when flavorUsesAutomaticPrimitivesSerialization(Toml, Flavor):
    const
      flavorName = typetraits.name(Flavor)
      typeName = typetraits.name(TargetType)
    {.error: flavorName & ": please set automaticPrimitivesSerialization to false".}
  template flavorAutoSerializationWrite*(_: type Toml, _: type Flavor, _: distinct type TargetType): bool = true

template setAutoSerialization*(Flavor: type SerializationFormat, TargetType: distinct type) =
  setAutoSerializationRead(Flavor, TargetType)
  setAutoSerializationWrite(Flavor, TargetType)
