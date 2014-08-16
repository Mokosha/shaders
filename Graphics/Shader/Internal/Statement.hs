module Graphics.Shader.Internal.Statement (
  Statement,
  addStatement,
  emptyStmt, assignStmt,
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

emptyStmt :: Statement
emptyStmt = StatementList []

assignStmt :: ShaderVar a -> Expr a -> Statement
assignStmt var expr = Assignment var expr
