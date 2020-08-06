import
  tables, strutils, typetraits, macros, strformat,
  faststreams/inputs, serialization/[object_serialization, errors]

type
  TomlReader* = object
