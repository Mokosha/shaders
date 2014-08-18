module Graphics.Shader.Program (
  ShaderState(..), Shader,
  mkVertexShader,
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
  inputs :: ShaderAttributes a,
  outputs :: ShaderAttributes b,
  shaderStatements :: Statement
}

data ShaderAttributes a = ShaderAttributes {
  numAttributes :: Int,
  getAttrib :: Int -> ShaderVarRep
}

type PosType = ShaderVec4 Float
mkVertexShader :: ShaderAttributes a -> 
                  (ShaderAttributes a -> Shader (ShaderVar PosType, ShaderAttributes b)) ->
                  ShaderProgram a b
mkVertexShader attribs shaderFn = let
  input = attribs { getAttrib = \idx -> (\sa -> sa { varID = idx }) (getAttrib attribs idx) }
  shader = shaderFn input
  ((posVar, out), st) = runState shader $ ShaderState {
    nextVarID = (numAttributes attribs),
    stmt = emptyStmt
  }
  in
   ShaderProgram { inputs = input, outputs = out, shaderStatements = stmt st }
