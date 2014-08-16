module Graphics.Shader.Internal.Variable (
  ShaderVec2, ShaderVec3, ShaderVec4,
  ShaderVar, ShaderVarID, ShaderVarRep(..), ShaderVarType,
  newLocal,
) where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

data ShaderVec2 a = ShaderVec2 !a !a       deriving (Ord, Eq, Show, Read)
data ShaderVec3 a = ShaderVec3 !a !a !a    deriving (Ord, Eq, Show, Read)
data ShaderVec4 a = ShaderVec4 !a !a !a !a deriving (Ord, Eq, Show, Read)

data ConstValue = ConstBool !Bool
                | ConstInt !Int
                | ConstFloat !Float
                | ConstDouble !Double
                | ConstVec2 !(ShaderVec2 ConstValue)
                | ConstVec3 !(ShaderVec3 ConstValue)
                | ConstVec4 !(ShaderVec4 ConstValue)
                deriving (Ord, Eq, Show, Read)

type ShaderVarID = Int

data ShaderVarType = BoolTy
                   | IntTy
                   | FloatTy
                   | DoubleTy
                   | Vec2Ty ShaderVarType
                   | Vec3Ty ShaderVarType
                   | Vec4Ty ShaderVarType
                     deriving (Ord, Eq, Show, Read)

data ShaderVarScope = ConstScope ConstValue
                    | LocalScope
                    | AttributeScope
                    | UniformScope
                     deriving (Ord, Eq, Show, Read)

data ShaderVarRep = ShaderVarRep {
  varName :: String,
  ty :: ShaderVarType,
  varID :: ShaderVarID,
  scope :: ShaderVarScope
} deriving (Show, Eq)

type ShaderVar a = ShaderVarRep

newVar :: String -> ShaderVarType -> ShaderVarScope -> ShaderVarID -> ShaderVar a
newVar name varTy varScope varid =
  ShaderVarRep { varName = name, ty = varTy, scope = varScope, varID = varid }

newLocal :: ShaderVarType -> ShaderVarID -> ShaderVar a
newLocal varTy varid = newVar ("local_" ++ (show varid)) varTy LocalScope varid

newAttrib :: String -> ShaderVarType -> ShaderVarID -> ShaderVar a
newAttrib name varTy varid = newVar name varTy AttributeScope varid

newLocalBool :: ShaderVarID -> (ShaderVar Bool)
newLocalBool varid = newLocal BoolTy varid

newLocalFloat :: ShaderVarID -> (ShaderVar Float)
newLocalFloat varid = newLocal FloatTy varid
