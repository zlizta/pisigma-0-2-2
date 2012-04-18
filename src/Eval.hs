{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import Text.PrettyPrint.ANSI.Leijen (Doc,text)
import Data.Map

import Location
import Syntax
import Environment
import Values

newtype Eval a = Eval { unEval :: ReaderT Constraints (StateT Environment (ErrorT EvalError Identity)) a }
  deriving ( Monad
           , MonadError EvalError
           , MonadState Environment
           , MonadReader Constraints
           , Functor)

type EvalError   = String   -- Evaluation error message


data Pattern = PLabel Label deriving Show
   
data Constraint  = Constraint Neutral Pattern

data Constraints = Inconsistent | Constraints [Constraint]

emptyConstraints :: Constraints
emptyConstraints = Constraints []

pat2val :: Pattern -> Value
pat2val (PLabel l) = VLabel l


run :: Environment -> Eval a -> Either EvalError (a, Environment)
run e (Eval p) = runIdentity $ runErrorT $ runStateT (runReaderT p emptyConstraints) e

-- | Term t only sees all terms declared before itself
evalProg :: (Prog, Context) -> Eval Context
evalProg ([],g)                = return g
evalProg (Decl l x a : prog,g) = do_env_declare x (PrintInfo x True)  g (const (a,g)) >>= evalProg . (prog,) . snd
evalProg (Defn l x t : prog,g) = getContextIndex l x g >>= (`do_env_update` (t,g)) >> evalProg (prog,g)

-- | Evaluation in a locally updated environment.
letEval :: Index -> Closure Term -> Eval a -> Eval a
letEval i t p =
    do t0 <- do_env_lookup i
       do_env_update i t
       a  <- p
       do_env_update i t0
       return a

do_env_declare:: Name -> PrintInfo -> Context -> (Int -> Closure Type) -> Eval (Int, Context)
do_env_declare x p g f =
    do e <- getEnvironment
       let i = size e
       putEnvironment $ insert i (cUndefined i,f i,p) e
       return (i,extendContext g x i)

do_env_declare'  x (a,g)    = do_env_declare x (PrintInfo x False) g (const (a,g))
do_env_declare'' x    g ag' = do_env_declare x (PrintInfo x False) g (const ag')

do_env_lookup :: Index -> Eval (Closure Term)
do_env_lookup i =  getEnvironment >>= return . (env_lookup_term i)

do_env_update i t =
    do  e <- getEnvironment
        let e' = env_update_term i t e
        putEnvironment e'
        return ()

-- | Gets the current evaluation environment.
getEnvironment :: Eval Environment
getEnvironment = get

-- | Redefines the current evaluation environment.
putEnvironment :: Environment -> Eval ()
putEnvironment = put

-- | Gets the index associated to name x in context g.
getContextIndex :: Location -> Name -> Context -> Eval Index
getContextIndex l x (Context g) = case Prelude.lookup x g of
                      Just i -> return i
                      Nothing -> throwError $ locMessage l
                                 ++ "\nUndefined variable: "
                                 ++ x
                                 