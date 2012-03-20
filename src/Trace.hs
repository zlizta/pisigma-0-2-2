module Trace where

import Prelude  hiding (putStrLn)
import qualified Data.Set as Set
import Control.Monad.Error
import Text.Parsec.Error

import Location
import Error
import Parser
import Lexer
import Syntax
import Environment
import Values
import Eval
import Equal
import Check
import Debug
import Constraints
import Evaluate
import PSIO
import Symbols
import Smart
import Version
import Pretty
import Tools

program_parse :: Either ParseError Prog
program_parse = 
        parse sProg "Trace" 
        $ unlines 
           [ "Unit : Type;"
           , "Unit = { unit };"
           , "Nat : Type;"
           , "Nat = (l : { z s }) * case l of {z -> Unit | s -> Rec [Nat] };"
           ]
        
trace :: Either EvalError (Context, Environment)       
trace = case program_parse of
            Left  err  -> throwError $ "Parse error" ++ show err
            Right prog -> run emptyEnvironment (t_checkProg (prog,emptyContext))       

t_check :: Closure Term -> Closure Type -> Eval ()
t_check closureTerm@(term, context)
        closureType@(type', context') 
        =
        traceA "check" [shD "term" term,shD "G" context, shD "type" type',shD "G'" context'] (check closureTerm closureType) 

t_checkProg :: Closure Prog -> Eval Context
t_checkProg st = t_checkProg' Set.empty Set.empty st

t_checkProg' :: Set Name -> Set Name -> Closure Prog -> Eval Context
t_checkProg' decls defns ([],g) = return g
t_checkProg' decls defns ((Decl _ x a):prg,g) =
    do t_check (a,g) cType -- ^ Checks that a is a valid type in context g
       (_,g') <- do_env_declare' x (a,g)
       if x `Set.member` decls
        then throwError $ "Variable " ++ x ++ " declared twice in the same block."
        else t_checkProg' (Set.insert x decls) defns (prg,g')
t_checkProg' decls defns ((Defn l x t):prg,g) =
    do a <- inferVar l (x,g)  -- ^ Looks up the type of variable x in context g
       t_check (t,g) a
       i <- getContextIndex l x g
       do_env_update i (t,g) 
       if not (x `Set.member` decls) 
        then throwError $ "Name " ++ x ++ " not declared in the same context as its definition."
        else t_checkProg' decls (Set.insert x defns) (prg,g)
        
{- In check.hs
-- Copy this and change check into check_0

check :: Closure Term -> Closure Type -> Eval ()
check closureTerm@(term, context)
        closureType@(type', context') 
        =
        traceA "check" [shD "term" term,shD "G" context, shD "type" type',shD "G'" context'] (check_0 closureTerm closureType)    

-- Idem with 
check' ct@(term,context) v = traceA "check'" [shD "term" term , shD "G" context, shD "v" v] (check'_0 ct v)


infer closureTerm@(term,context) =
        traceA "infer" [shD "term" term,shD "G" context] (infer_0 closureTerm) 
        
-}