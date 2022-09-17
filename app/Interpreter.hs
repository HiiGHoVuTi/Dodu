module Interpreter where

import Lambda
import Data.Map
import Data.Text


builtins :: Map Text (LambdaExpr -> LambdaExpr)
builtins = fromList [] -- TODO: Add builtin functions


evalExpr :: LEnv -> LambdaExpr -> LambdaExpr
evalExpr env expr =
  case expr of
    LVal (LRat r) -> LVal (LRat r)
    LVal (LList l) -> LVal (LList (fmap (evalExpr env) l))
    LVar var -> env ! var

    -- applications
    LApp (LVal a) _    -> evalExpr env (LVal a)
    LApp (LVar v) expr
      | v `member` env -> evalExpr env (LApp (env ! v) (evalExpr env expr))

    LApp (LVar v) expr
      | v `member` builtins -> (builtins ! v) (evalExpr env expr)

    LApp (LAbs x body) expr ->
      let res = evalExpr env expr in
        evalExpr (insert x res env) body


    _ -> error "sale rat"
