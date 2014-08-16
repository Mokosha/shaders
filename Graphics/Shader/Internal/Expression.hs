module Graphics.Shader.Internal.Expression (
  Expr, Expression
) where

--------------------------------------------------------------------------------
import Graphics.Shader.Internal.Variable

--------------------------------------------------------------------------------

data Op = Add
        | Subtract
        | Multiple
        | Divide

data Expression = VarExpr ShaderVarRep
                | BinOp Op Expression Expression

type Expr a = Expression

class Addable a where
  addE :: Expr a -> Expr a -> Expr a
  addE e1 e2 = BinOp Add e1 e2

instance Addable Int
instance Addable Float
instance Addable Double

class Subtractable a where
  subtractE :: Expr a -> Expr a -> Expr a
  subtractE e1 e2 = BinOp Subtract e1 e2

instance Subtractable Int
instance Subtractable Float
instance Subtractable Double
