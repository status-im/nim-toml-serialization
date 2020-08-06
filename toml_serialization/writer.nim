import
  typetraits,
  faststreams/[outputs, textio], serialization

type
  TomlWriter* = object
    stream*: OutputStream
