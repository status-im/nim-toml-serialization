# toml-serialization
# Copyright (c) 2020-2026 Status Research & Development GmbH
# Licensed and distributed under either of
#   * MIT license: [LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT
#   * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
# at your option. This file may not be copied, modified, or distributed except according to those terms.

import
  toml_serialization/[reader, writer, types, desc, format, decoder]

export
  desc, reader, writer, types, format, decoder

Toml.setReader TomlReader
Toml.setWriter TomlWriter, PreferredOutput = string

template supports*(_: type Toml, T: type): bool =
  # The TOML format should support every type
  # when not at top level ... :)
  true

Toml.setDecoder(Toml)

createTomlFlavor Toml_v100,
  automaticObjectSerialization = true,
  automaticPrimitivesSerialization = true,
  runtimeFlags = {},
  version = TomlVersion_v100
