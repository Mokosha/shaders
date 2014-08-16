module Graphics.Shader.Internal.Statement (
) where

--------------------------------------------------------------------------------
import Graphics.Shader.Internal.Variable
import Graphics.Shader.Internal.Expression

--------------------------------------------------------------------------------

data Statement = StatementList [Statement]
               | Assignment ShaderVarRep Expression
