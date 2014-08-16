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
mkConst val = SConst val

data Vec2 a = Vec2 { getX :: a, getY :: a }
data Vec3 a = Vec3 { getXY :: Vec2 a, getZ :: a }
data Vec4 a = Vec4 { getXYZ :: Vec3 a, getW :: a }

class ShaderNumeric a

instance ShaderNumeric Int
instance ShaderNumeric Float
instance ShaderNumeric Double
instance ShaderNumeric a => (ShaderNumeric (Vec2 a))
instance ShaderNumeric a => (ShaderNumeric (Vec3 a))
instance ShaderNumeric a => (ShaderNumeric (Vec4 a))
