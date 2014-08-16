module Shader.Internal.Statement (
) where

--------------------------------------------------------------------------------
import Shader.Internal.Variable
import Shader.Internal.Expression

--------------------------------------------------------------------------------

data Statement = StatementList [Statement]
               | Assignment Shader
