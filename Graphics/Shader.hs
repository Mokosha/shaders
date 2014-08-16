module Graphics.Shader (

) where

--------------------------------------------------------------------------------
import Control.Monad.State

import Graphics.Shader.Internal.Expression
import Graphics.Shader.Internal.Statement
import Graphics.Shader.Internal.Variable
import Graphics.Shader.Program
--------------------------------------------------------------------------------

assign :: Expr a -> Shader (ShaderVar a)
assign expr = do
  shaderState <- get
  let newVar = newLocal (getType expr) (nextVarID shaderState)
      newStmt = addStatement (assignStmt newVar expr) (stmt shaderState)
  put $ shaderState { nextVarID = (nextVarID shaderState + 1), stmt = newStmt }
  return newVar
