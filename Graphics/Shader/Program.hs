module Graphics.Shader.Program (
  ShaderState(..), Shader,
  mkVertexShader2_2,
) where

--------------------------------------------------------------------------------
import Control.Monad.State

import Graphics.Shader.Internal.Statement
import Graphics.Shader.Internal.Variable

--------------------------------------------------------------------------------

data ShaderState = ShaderState {
  nextVarID :: ShaderVarID,
  stmt :: Statement
}

type Shader a = State ShaderState a

data ShaderProgram a b = ShaderProgram {
  inputs :: a,
  outputs :: b,
  shaderStatements :: Statement
}

type PosType = ShaderVec4 Float
mkVertexShader2_2 :: ShaderVar a -> ShaderVar b ->
                     (ShaderVar a -> ShaderVar b -> Shader (ShaderVar PosType, ShaderVar c)) ->
                     ShaderProgram (ShaderVar a, ShaderVar b) (ShaderVar PosType, ShaderVar c)
mkVertexShader2_2 attrib1 attrib2 shaderFn = let
  a1 = attrib1 { varID = 0 }
  a2 = attrib2 { varID = 1 }
  shader = shaderFn a1 a2
  (out, st) = runState shader $ ShaderState { nextVarID = 2, stmt = emptyStmt }
  in
   ShaderProgram { inputs = (a1, a2), outputs = out, shaderStatements = stmt st }
