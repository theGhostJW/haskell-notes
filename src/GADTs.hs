module GADTs where

import           Foundation.Extended
import qualified Prelude
--  !!!!   GADTs  !!!!!
data Expr a where
  I   :: Int  -> Expr Int
  B   :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

eval :: Expr a -> a
eval (I n)       = n
eval (B b)       = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

demo1 = eval $ Mul (I 5) (I 10)
-- >> 50

{-
demo2 = eval $ Mul (I 5) (B False)

src\GADTs.hs:23:27-33: error:
    * Couldn't match type `Bool' with `Int'
      Expected type: Expr Int
        Actual type: Expr Bool
    * In the second argument of `Mul', namely `(B False)'
      In the second argument of `($)', namely `Mul (I 5) (B False)'
      In the expression: eval $ Mul (I 5) (B False)
   |
23 | demo2 = eval $ Mul (I 5) (B False)
-}
