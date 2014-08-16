module Shader.Internal.Variable (
  ShaderVar
) where

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

type ShaderVarID = Int

data ShaderVar a = SConst a
                 | SLocal
                 | SAttribute String
                 | SUniform String
                 | SPredefined String

mkConst :: a -> ShaderVar a
mkConst val = Const val

newtype Vec2 a = Vec2 { getX :: a, getY :: a }
newtype Vec3 a = Vec3 { getX :: a, getY :: a, getZ :: a }
newtype Vec4 a = Vec4 { getX :: a, getY :: a, getZ :: a, getW :: a }
