{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Check where

import Prelude hiding ( length )
import Control.Arrow  ( first )
import Control.Monad
import Control.Monad.Error
import Data.Maybe (fromJust)
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import Location
import Syntax
import Environment
import Values
import Eval
import Equal
import Evaluate
import Constraints
import Pretty
import Error

type Set        = Set.Set

cType :: Closure Type
cType = (Type Unknown, emptyContext)

cLabel :: Label -> Closure Term
cLabel x = (Label Unknown x, emptyContext)

checkProg :: Closure Prog -> Eval Context
checkProg st = checkProg' Set.empty Set.empty st

checkProg' :: Set Name -> Set Name -> Closure Prog -> Eval Context
checkProg' decls defns ([],g) = return g
checkProg' decls defns ((Decl _ x a):prg,g) =
    do check (a,g) cType -- ^ Checks that a is a valid type in context g
       (_,g') <- do_env_declare' x (a,g)
       if x `Set.member` decls
        then throwError $ "Variable " ++ x ++ " declared twice in the same block."
        else checkProg' (Set.insert x decls) defns (prg,g')
checkProg' decls defns ((Defn l x t):prg,g) =
    do a <- inferVar l (x,g)  -- ^ Looks up the type of variable x in context g
       check (t,g) a
       i <- getContextIndex l x g
       do_env_update i (t,g) 
       if not (x `Set.member` decls) 
        then throwError $ "Name " ++ x ++ " not declared in the same context as its definition." -- ^ TO DO: Give an example of a program where this error is reached.
        else checkProg' decls (Set.insert x defns) (prg,g)
        
-- | Takes a term and an (unevaluated) type. We first
-- handle the cases that may potentially change the
-- environment. If none of those cases match, we can
-- safely evaluate the type and call check'.
check :: Closure Term -> Closure Type -> Eval ()
check (Let _ p t,g) c = checkProg (p,g) >>= (`check`  c) . (t,)
check (Split _ t (x,(y,u)),g) c
 | x==y      = throwErrorc t $ "Repeated variables in a split"
 | otherwise =
    do sigab <- inferEval (t,g)
       case sigab of
         (VQ Sigma ((a,(z,b)),s)) ->
             do t'  <- eval (t,g)
                (_,g')  <- do_env_declare'' x g (a,s)
                b'  <- subst (z,(b,s)) (Var Unknown x, g')
                (_,g'') <- do_env_declare'' y g' b'
                case t' of
                  VNeutral (NVar i) ->
                      letEval i 
                        ((Pair Unknown (Var Unknown x) (Var Unknown y), g'')) (check (u,g'') c)
                  _ -> check (u,g'') c
                       -- instead of failing, we just omit the assignment.
         _ -> expectedButFound (t,g) msg1 sigab msg2
                where
                  msg1 = "sigma type" :: String
                  msg2 = "split"      :: String
       
check gt @ (Case _ t lus,g) c =
    do enum <- inferEval(t,g)
       case enum of
         VEnum ls ->
             let ls' = map fst lus
             in if ls /= ls'
                then
                  throwErrorc gt   $  text $ "Labels don't match."
                                             ++ "\nProvided labels: " ++  show ls
                                             ++ "\nExpected labels: " ++  show ls'
                                             ++ "\n(Case)\n"  
                -- set equivalence would be sufficent
                else mapM_ (\ (l,u) -> addContraint (t,g) (PLabel l) (check (u,g) c)) lus
                     
         _ -> expectedButFound (t,g) msg1 enum msg2
                where
                  msg1 = "enum type" :: String
                  msg2 = "case"      :: String
check (Force _ t,g) (a,s) = check (t,g) (Lift Unknown a, s)
check (Unfold _ t (x,u),g) c =
    do rec <- inferEval (t,g)
       case rec of
         VRec (a,s) ->
             do t'      <- eval (t,g)
                (_,g')  <- do_env_declare'' x g (Force Unknown a,s)
                case t' of
                  (VNeutral (NVar i)) ->
                      letEval i ((Fold Unknown (Var Unknown x), g'))
                                (check (u,g') c)
                  _ -> check (u,g') c
         _ -> expectedButFound (t,g) msg1 rec msg2
                where
                  msg1 = "rec type" :: String
                  msg2 = "unfold"   :: String

check closureTerm@(term, context)
        closureType@(type', context') 
        = eval closureType >>= check' closureTerm
        

check' :: Closure Term -> Value -> Eval ()
check' (Lambda _ (x,t),g) (VQ Pi ((a,(y,b)),s)) =
    do (i,g') <- do_env_declare'' x g (a,s)
       let s' = extendContext s y i
       check (t,g') (b,s')
check' gt @ (Lambda _ _,_) a = expected gt a "Type checking - Lambda"
check' (Pair _ t u,g) (VQ Sigma ((a,(y,b)),s)) =
    do check (t,g) (a,s)
       b' <- subst (y,(b,s)) (t,g)
       check (u,g) b'
check' gt @ (Pair _ _ _, _) a =
    expected gt a "Pair"
-- Labels cannot be inferred because there is no way to know  what the other labels are.
check'    (Label _ l,_)   (VEnum ls) | l `elem` ls = return ()
check' gt@(Label _ _,_)    a                       = expected gt a "Type checking - Label"
check'    (Box _ t,g)     (VLift a)                = check (t,g) a
check'    (Fold _ t,g)    (VRec (a,s))             = check' (t,g) =<< eval (Force Unknown a, s)
check'     closureTerm     value                   = do 
                                                        value' <- inferEval closureTerm
                                                        catchError (eq value value') $ \ s -> expectedButFound closureTerm value value' "Type Checking"
       
-- | Infers the type of a variable by looking up its type in the associated context.
inferVar :: Location -> Closure Name -> Eval (Closure Type)
inferVar l (x,g) =
    do
    i <- getContextIndex l x g
    getEnvironment >>= return . p32 . fromJust . (Map.lookup i) where p32 (_,a,_) = a

                               
   
infer :: Closure Term -> Eval (Closure Type)
infer (Var l x,g) = inferVar l (x,g)
infer (Let _ tel t,g) =
    do g' <- checkProg (tel,g)
       infer (t,g')
       
infer (Type _,_) = return cType

infer (Q _ _ (a,(x,b)),g) =
    do check (a,g) cType
       (_,g') <- do_env_declare'' x g (a,g)
       check (b,g') cType
       return cType

infer (App t u,g) =
    do piab <- inferEval (t,g)
       case piab of
         (VQ Pi ((a,(x,b)),s)) -> do check (u,g) (a,s)
                                     subst (x,(b,s)) (u,g)
         _ -> expectedButFound (t,g) msg1 piab msg2
                where
                  msg1 = "pi type" :: String
                  msg2 = "App"     :: String

infer (t@(Enum _ ls),_) =
    if List.length (List.nub ls) < List.length ls
      then throwErrorc t "Duplicate labels in enum type"
      else return cType

infer (Box  _ t,g) = liftM (first $ Lift Unknown) (infer (t,g))

infer (Fold  _ t,g) =
  liftM (first $ Rec Unknown . Box Unknown) (infer (t,g))

infer (Force _ t,g) =
    do a <- inferEval (t,g)
       case a of
         VLift b -> return b
         _       -> expectedButFound (t,g) msg1 a msg2
                      where
                        msg1 = "lifted type" :: String
                        msg2 = "Force"       :: String

infer (Lift _ a,g) =
    do check (a,g) cType
       return cType

infer (Rec _ a,g) =
    do check (a,g) (Lift Unknown (Type Unknown), emptyContext)
       return cType
       
infer gt = throwErrorc gt $ "Not inferable" <$> "(infer)"
     
-- | Infers a type and evaluates it.
inferEval :: Closure Term -> Eval Value
inferEval gt = infer gt >>= eval
