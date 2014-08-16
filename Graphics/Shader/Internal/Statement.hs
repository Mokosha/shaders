module Graphics.Shader.Internal.Statement (
  Statement,
  addStatement,
  assignStmt,
) where

--------------------------------------------------------------------------------
import Graphics.Shader.Internal.Variable
import Graphics.Shader.Internal.Expression

--------------------------------------------------------------------------------

data Statement = StatementList [Statement]
               | Assignment ShaderVarRep Expression

addStatement :: Statement -> Statement -> Statement
addStatement stmt (StatementList stmts) = StatementList (stmt : stmts)
addStatement stmt x = StatementList [stmt, x]

assignStmt :: ShaderVar a -> Expr a -> Statement
assignStmt var expr = Assignment var expr
