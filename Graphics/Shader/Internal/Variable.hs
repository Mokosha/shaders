module Graphics.Shader.Internal.Variable (
  ShaderVar, ShaderVarRep,
  newLocalBool, newLocalFloat
) where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

type ShaderVarID = Int

data ShaderVarType = BoolTy
                   | IntTy
                   | FloatTy
                   | DoubleTy
                   | Vec2Ty ShaderVarType
                   | Vec3Ty ShaderVarType
                   | Vec4Ty ShaderVarType
                     deriving (Ord, Eq, Show, Read)

data ShaderVarScope = ConstScope
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

newLocalBool :: ShaderVarID -> (ShaderVar Bool, ShaderVarID)
newLocalBool varid = (
  ShaderVarRep { ty = BoolTy, varID = varid, scope = LocalScope },
  varid + 1)

newLocalFloat :: ShaderVarID -> (ShaderVar Float, ShaderVarID)
newLocalFloat varid = (
  ShaderVarRep { ty = FloatTy, varID = varid, scope = LocalScope },
  varid + 1)
