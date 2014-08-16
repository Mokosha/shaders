module Graphics.Shader.Internal.Variable (
  ShaderVar, ShaderVarID, ShaderVarRep(..), ShaderVarType(..),
  newLocal,
) where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

data ConstValue = ConstBool !Bool
                | ConstInt !Int
                | ConstFloat !Float
                | ConstDouble !Double
                | ConstVec2 !ConstValue !ConstValue
                | ConstVec3 !ConstValue !ConstValue !ConstValue
                | ConstVec4 !ConstValue !ConstValue !ConstValue !ConstValue
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
  ty :: ShaderVarType,
  varID :: ShaderVarID,
  scope :: ShaderVarScope
} deriving (Show, Eq)

type ShaderVar a = ShaderVarRep

newVar :: ShaderVarType -> ShaderVarScope -> ShaderVarID -> ShaderVar a
newVar varTy varScope varid = ShaderVarRep { ty = varTy, scope = varScope, varID = varid }

newLocal :: ShaderVarType -> ShaderVarID -> ShaderVar a
newLocal varTy varid = newVar varTy LocalScope varid

newLocalBool :: ShaderVarID -> (ShaderVar Bool)
newLocalBool varid = newLocal BoolTy varid

newLocalFloat :: ShaderVarID -> (ShaderVar Float)
newLocalFloat varid = newLocal FloatTy varid
