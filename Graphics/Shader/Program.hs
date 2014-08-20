module Graphics.Shader.Program (
  ShaderState(..), Shader,
  mkVertexShader,
  mkFragmentShader,
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

emptyAttribs :: ShaderAttributes ()
emptyAttribs = ShaderAttributes { numAttributes = 0, getAttrib = error "No attributes!" }

type Vec4f = ShaderVec4 Float
mkVertexShader :: ShaderAttributes a -> 
                  (ShaderAttributes a -> Shader (ShaderVar Vec4f, ShaderAttributes b)) ->
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


mkFragmentShader :: ShaderAttributes a ->
                  (ShaderAttributes a -> Shader (ShaderVar Vec4f)) -> ShaderProgram a ()
mkFragmentShader varyings shaderFn = let
  input = varyings { getAttrib = \idx -> (\sa -> sa { varID = idx }) (getAttrib varyings idx) }
  shader = shaderFn input
  (_, st) = runState shader $ ShaderState {
    nextVarID = (numAttributes varyings),
    stmt = emptyStmt
  }
  in
   ShaderProgram { inputs = input, outputs = emptyAttribs, shaderStatements = stmt st }
