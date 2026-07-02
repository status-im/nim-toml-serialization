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

type
  TomlVersion* = tuple[major: int, minor: int, patch: int]

const
  TomlVersion_v100* = (1,0,0)
  TomlVersion_v110* = (1,1,0)
  Toml_v110_Flags* = {
    TomlInlineTableNewline,
    TomlInlineTableTrailingComma,
    TomlHexEscape,
    TomlHourMinute,
    TomlStrictComma,
  }

template flavorRuntimeFlags*(_: type Toml): set[TomlFlag] = Toml_v110_Flags
template flavorUsesAutomaticObjectSerialization*(_: type Toml): bool = true
template flavorUsesAutomaticPrimitivesSerialization*(_: type Toml): bool = true
template flavorAutoSerializationRead*(_: type Toml, _: distinct type): bool = true
template flavorAutoSerializationWrite*(_: type Toml, _: distinct type): bool = true
template flavorTomlVersion*(_: type Toml): TomlVersion = TomlVersion_v110

template createTomlFlavor*(FlavorName: untyped,
                           mimeTypeValue = "application/toml",
                           automaticObjectSerialization = false,
                           automaticPrimitivesSerialization = true,
                           runtimeFlags: set[TomlFlag] = Toml_v110_Flags,
                           version: TomlVersion = TomlVersion_v110) {.dirty.} =

  createFlavor(Toml, FlavorName, mimeTypeValue)
  template flavorRuntimeFlags*(_: type FlavorName): set[TomlFlag] =
    when version >= TomlVersion_v110: runtimeFlags + Toml_v110_Flags
    else: runtimeFlags
  template flavorUsesAutomaticObjectSerialization*(_: type FlavorName): bool = automaticObjectSerialization
  template flavorUsesAutomaticPrimitivesSerialization*(_: type FlavorName): bool = automaticPrimitivesSerialization
  template flavorAutoSerializationRead*(_: type FlavorName, _: distinct type): bool = automaticPrimitivesSerialization
  template flavorAutoSerializationWrite*(_: type FlavorName, _: distinct type): bool = automaticPrimitivesSerialization
  template flavorTomlVersion*(_: type FlavorName): TomlVersion = version

template setAutoSerializationRead*(Flavor: type Toml, TargetType: distinct type) =
  mixin flavorUsesAutomaticPrimitivesSerialization
  when flavorUsesAutomaticPrimitivesSerialization(Flavor):
    const
      flavorName = typetraits.name(Flavor)
      typeName = typetraits.name(TargetType)
    {.error: flavorName & ": please set automaticPrimitivesSerialization to false".}
  template flavorAutoSerializationRead*(_: type Flavor, _: distinct type TargetType): bool = true

template setAutoSerializationWrite*(Flavor: type Toml, TargetType: distinct type) =
  mixin flavorUsesAutomaticPrimitivesSerialization
  when flavorUsesAutomaticPrimitivesSerialization(Flavor):
    const
      flavorName = typetraits.name(Flavor)
      typeName = typetraits.name(TargetType)
    {.error: flavorName & ": please set automaticPrimitivesSerialization to false".}
  template flavorAutoSerializationWrite*(_: type Flavor, _: distinct type TargetType): bool = true

template setAutoSerialization*(Flavor: type Toml, TargetType: distinct type) =
  setAutoSerializationRead(Flavor, TargetType)
  setAutoSerializationWrite(Flavor, TargetType)
