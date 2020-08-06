import
  serialization, toml_serialization/[reader, writer]

export
  serialization, reader, writer

serializationFormat Toml,
                    Reader = TomlReader,
                    Writer = TomlWriter,
                    PreferedOutput = string,
                    mimeType = "application/toml"

template supports*(_: type Toml, T: type): bool =
  # The TOML format should support every type
  true
