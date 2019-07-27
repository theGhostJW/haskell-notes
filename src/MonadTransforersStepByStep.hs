module MonadTransforersStepByStep where


-- Moand transformers Step By Step
-- https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf


-- pp.2 -- interpreter for a simple programming language

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map
import Prelude

type Name   =  String                -- variable names

data Exp    =  Lit Integer           -- literlal
            |  Var Name              -- var name
            |  Plus Exp Exp          -- addition
            |  Abs Name Exp          -- look up value from name
            |  App Exp Exp           -- apply function mutate
            deriving (Show)

data Value  =  IntVal Integer        -- integer
            |  FunVal Env Name Exp   -- functoion val vars / variable name / expression
            deriving (Show)

type Env = Map.Map Name Value -- mapping from names to values

{- DOn't Compile anymore due to chanes in ghc
-- no monads

-- my function to make the warning go away
evalInt ::  Env -> Exp -> Integer
evalInt = undefined


-- interpreter
eval0                   ::  Env -> Exp -> Value
eval0 env (Lit i)       =   IntVal i
eval0 env (Var n)       =   fromJust (Map.lookup n env)
eval0 env (Plus e1 e2)  =   let 
                              --  will throw exception if not int value
                              IntVal i1  = eval0 env e1
                              IntVal i2  = eval0 env e2
                            in 
                              IntVal (i1 + i2)

eval0 env (Abs n e)     =   FunVal env n e
eval0 env (App e1 e2)   =   let  
                              val1  = eval0 env e1
                              val2  = eval0 env e2
                            in 
                              case val1 of
                                FunVal env' n body -> eval0 (Map.insert n val2 env') body
                                -- will throw exception if not function

exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)
exmp0 = eval0 Map.empty exampleExp 


-- ╬╗> exampleExp
-- IntVal 18

-- 2.1 Converting to Monadic Style

type Eval1 alpha = Identity alpha

runEval1 ::  Eval1 alpha -> alpha
runEval1 = runIdentity


eval1                   ::  Env -> Exp -> Identity Value
eval1 env (Lit i)       =   pure $ IntVal i
eval1 env (Var n)       =   maybe (fail ("undefined variable: " ++ n)) pure $ Map.lookup n env
eval1 env (Plus e1 e2)  =   do 
                                (IntVal i1)  <- eval1 env e1
                                (IntVal i2)  <- eval1 env e2
                                pure . IntVal $ i1 + i2

eval1 env (Abs n e)     =   pure $ FunVal env n e
eval1 env (App e1 e2)   =   do  
                              val1  <- eval1 env e1
                              val2  <- eval1 env e2
                              case val1 of
                                  FunVal env' n body ->
                                      eval1 (Map.insert n val2 env') body

exmp1 = runIdentity $ eval1 Map.empty exampleExp 


-- 2.2 Adding Error Handling

type Eval2 alpha = ExceptT String Identity alpha

runEval2 :: Eval2 alpha -> Either String alpha
runEval2 ev = runIdentity $ runExceptT ev

{- no longer compiles since does not have Monad Fail -}

eval2a                   ::  Env -> Exp -> Eval2 Value
eval2a env (Lit i)       =   pure $ IntVal i
eval2a env (Var n)       =   maybe (fail ("undefined variable: " ++ n)) pure $ Map.lookup n env
eval2a env (Plus e1 e2)  =   do  (IntVal i1)  <- eval2a env e1
                                 (IntVal i2)  <- eval2a env e2
                                 pure $ IntVal (i1 + i2)
eval2a env (Abs n e)     =   pure $ FunVal env n e
eval2a env (App e1 e2)   =   do  val1  <- eval2a env e1
                                 val2  <- eval2a env e2
                                 case val1 of
                                    FunVal env' n body ->
                                       eval2a (Map.insert n val2 env') body



patternMatchErr = Plus (Lit 1) (Abs "x" (Var "x"))

{-
- no longer compiles since does not have Monad Fail

demoFail = runEval2 $ eval2a Map.empty patternMatchErr

undefinedVar = Var "x"
demoFail2 = runEval2 $ eval2a Map.empty undefinedVar
-}

-- ╬╗> demoFail
-- *** Exception: Pattern match failure in do expression
-- ╬╗> demoFail2
-- *** Exception: undefined variable: x


eval2b                   ::  Env -> Exp -> Eval2 Value
eval2b env (Lit i)       =   pure $ IntVal i
eval2b env (Var n)       =   maybe (throwError ("undefined variable: " ++ n)) pure $ Map.lookup n env
eval2b env (Plus e1 e2)  =   do  
                                e1'  <- eval2b env e1
                                e2'  <- eval2b env e2
                                case (e1', e2') of
                                  (IntVal i1, IntVal i2) -> pure $ IntVal (i1 + i2)
                                  _ -> throwError "type error in addition"

eval2b env (Abs n e)     =   pure $ FunVal env n e
eval2b env (App e1 e2)   =   do  val1  <- eval2b env e1
                                 val2  <- eval2b env e2
                                 case val1 of
                                    FunVal env' n body -> eval2b (Map.insert n val2 env') body
                                    _ -> throwError "type error in application"

demoFailb = runEval2 (eval2b Map.empty (Plus (Lit 1) (Abs "x" (Var "x")))) 
demoFail2b = runEval2 (eval2b Map.empty (Var "x")) 


-- 2.3 Hiding the Environment

type Eval3 alpha = ReaderT Env (ExceptT String Identity) alpha

runEval3 :: Env -> Eval3 alpha -> Either String alpha
runEval3 ev m = runIdentity $ runExceptT $ runReaderT m ev

eval3 :: Exp -> Eval3 Value
eval3 = 
        \case
            Lit i ->  pure $ IntVal i
            Var n ->   do 
                        env <- ask
                        case Map.lookup n env of
                            Nothing -> throwError ("unbound variable: " ++ n)
                            Just val -> pure val

            Plus e1 e2  -> do  
                              e1'  <- eval3 e1
                              e2'  <- eval3 e2
                              case (e1', e2') of
                                    (IntVal i1, IntVal i2) ->
                                        pure $ IntVal (i1 + i2)
                                    _ -> throwError "type error in addition"

            Abs n e     ->  do  
                              env <- ask
                              pure $ FunVal env n e

            App e1 e2   ->   do  
                                val1  <- eval3 e1
                                val2  <- eval3 e2
                                case val1 of
                                  FunVal env' n body ->
                                    local (const (Map.insert n val2 env')) (eval3 body)
                                  _ -> throwError "type error in application"

demo3 = runEval3 Map.empty (eval3 exampleExp)


-- 2.4 Adding State

type Eval4 alpha = ReaderT Env (ExceptT String (StateT Integer Identity)) alpha


-- note the m always on inside of run stATEMENTS
runEval4 :: Env -> Integer -> Eval4 alpha -> (Either String alpha, Integer)
runEval4 env i m = runIdentity $ runStateT (runExceptT . runReaderT m $ env) i

-- log count for numbe of operations
-- use modify from monad state
tick :: (Num s, MonadState s m) => m ()
tick  = modify (+1)

-- eval4               ::  Exp -> Eval4 Value
eval4 (Lit i)       =   do  tick
                            pure $ IntVal i

eval4 (Var n)       =   do  tick
                            env <- ask
                            case Map.lookup n env of
                               Nothing -> throwError ("unbound variable: " ++ n)
                               Just val -> pure val

eval4 (Plus e1 e2)  =   do  tick
                            e1'  <- eval4 e1
                            e2'  <- eval4 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) ->
                                  pure $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"

eval4 (Abs n e)     =   do  tick
                            env <- ask
                            pure $ FunVal env n e

eval4 (App e1 e2)   =   do  tick
                            val1  <- eval4 e1
                            val2  <- eval4 e2
                            case val1 of
                                FunVal env' n body ->
                                  local (const (Map.insert n val2 env')) (eval4 body)
                                _ -> throwError "type error in application"

demo4 = runEval4 Map.empty 0  $ eval4 exampleExp
demoFail4 = runEval4 Map.empty 0 $ eval4 patternMatchErr

-- ╬╗> demo4
-- (Right (IntVal 18),8)

-- ╬╗> demoFail4
-- (Left "type error in addition",3)

type Eval4' alpha = ReaderT Env (StateT Integer (ExceptT String Identity)) alpha

runEval4' ::  Env -> Integer -> Eval4' alpha -> Either String (alpha, Integer)
runEval4' env st m  =  runIdentity (runExceptT (runStateT (runReaderT m env) st))

demo4' = runEval4' Map.empty 0 $ eval4 exampleExp
demoFail4' = runEval4' Map.empty 0 $ eval4 patternMatchErr

-- ╬╗> demo4'
-- Right (IntVal 18,8)

-- ╬╗> demoFail4'
-- Left "type error in addition"

-- 2.5 Adding Logging

type Eval5 alpha = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) alpha

runEval5 ::  Env -> Integer -> Eval5 alpha -> ((Either String alpha, [String]), Integer)
runEval5 env st m =  runIdentity $ runStateT (runWriterT . runExceptT $ runReaderT m env) st 

eval5               ::  Exp -> Eval5 Value
eval5 (Lit i)       =   do  tick
                            tell ["Lit"]
                            pure $ IntVal i

eval5 (Var n)       =   do  tick
                            tell ["Var " <> n]
                            env <- ask
                            case Map.lookup n env of
                               Nothing -> throwError ("unbound variable: " ++ n)
                               Just val -> pure val

eval5 (Plus e1 e2)  =   do  tick
                            tell ["Plus"]
                            e1'  <- eval5 e1
                            e2'  <- eval5 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) -> pure $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"

eval5 (Abs n e)     =   do  tick
                            tell ["ABS"]
                            env <- ask
                            pure $ FunVal env n e

eval5 (App e1 e2)   =   do  tick
                            tell ["App"]
                            val1  <- eval5 e1
                            val2  <- eval5 e2
                            case val1 of
                              FunVal env' n body ->
                                  local (const (Map.insert n val2 env'))
                                    (eval5 body)
                              _ -> throwError "type error in application"

demo5 = runEval5 Map.empty 0  $ eval5 exampleExp
demoFail5 = runEval5 Map.empty 0 $ eval5 patternMatchErr

-- What about I/O?
-- Fortunately, the monad transformer library provides us with the infrastructure to easily integrate I/O operations
-- into our framework: we simply substitute IO where we have used Identity!

type Eval6 alpha = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer IO))) alpha

runEval6 :: Env -> Integer -> Eval6 alpha -> IO ((Either String alpha, [String]), Integer)
runEval6 env st ev  = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval6               ::  Exp -> Eval6 Value
eval6 (Lit i)       =   do  tick
                            liftIO $ print i
                            return $ IntVal i

eval6 (Var n)       =   do  tick
                            tell [n]
                            env <- ask
                            case Map.lookup n env of
                               Nothing -> throwError ("unbound variable: " ++ n)
                               Just val -> return val

eval6 (Plus e1 e2)  =   do  tick
                            e1'  <- eval6 e1
                            e2'  <- eval6 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) ->
                                  return $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"

eval6 (Abs n e)     =   do  tick
                            env <- ask
                            return $ FunVal env n e

eval6 (App e1 e2)   =   do  tick
                            val1  <- eval6 e1
                            val2  <- eval6 e2
                            case val1 of
                              FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
                              _ -> throwError "type error in application"

demo6 = runEval6 Map.empty 0  $ eval6 exampleExp
demoFail6 = runEval6 Map.empty 0 $ eval6 patternMatchErr

-}