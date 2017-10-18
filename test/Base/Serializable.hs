module Base.Serializable where

class ClsSerializable a where
    serialize::a->String

data SerializableBase = SerializableBase {serializedData::String} deriving Show

instance ClsSerializable SerializableBase where
    serialize a = serializedData a
