module DataTypes where

-- gets parsed
data ClassDef = SimpleClassDef ClassName [FieldDef]
    deriving (Eq, Ord, Show, Read)

-- gets parsed
data FieldDef = SimpleFieldDef TypeName FieldName
    | ArrayFieldDef TypeName FieldName
    | MapFieldDef TypeName TypeName FieldName
    | ListFieldDef TypeName FieldName
    deriving (Eq, Ord, Show, Read)

type ClassName = String
type TypeName = String
type FieldName = String
