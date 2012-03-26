{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Evaluate where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe (fromJust)
import Data.Map (insert, size)
import qualified Data.Map as Map
import Control.Monad.Reader


import Syntax
import Environment
import Values
import Eval
import Equal







-- * Equality
instance Equal (Closure Term) where
    eq t u = do t' <- eval t
                u' <- eval u
                eq t' u'
                
instance Equal Value where
    eq (VNeutral t0) (VNeutral t1) = eq t0 t1
    eq (VQ ps0 ((a0,(x0,b0)),s0)) (VQ ps1 ((a1,(x1,b1)),s1)) | ps0 == ps1 = eq (a0,s0) (a1,s1) >> eq (x0,(b0,s0)) (x1,(b1,s1))
    eq (VLambda xt0) (VLambda xt1) = eq xt0 xt1
    eq (VPair ((t0,u0),s0)) (VPair ((t1,u1),s1)) =
        do eq (t0,s0) (t1,s1)
           eq (u0,s0) (u1,s1)
    eq (VBox b)  (VBox b')  = eq b b'
    eq (VLift a) (VLift a') = eq a a'
    eq (VRec a)  (VRec a')  = eq a a'
    eq (VFold a) (VFold a') = eq a a'
    eq v0 v1 | v0 == v1 = return () -- Type, Label, Enum
             | otherwise = throwError "Different values"
             
instance Equal Neutral where
    eq (NVar i0)            (NVar i1)            = eq i0 i1
    eq (NApp t0 u0)         (NApp t1 u1)         = eq t0 t1 >> eq u0 u1
    eq (NSplit t0 xyu0)     (NSplit t1 xyu1)     = eq t0 t1 >> eq xyu0 xyu1
    eq (NCase t0 (lus0,s0)) (NCase t1 (lus1,s1)) = eq t0 t1 >> eqBranches lus0 lus1
                                                   where eqBranches []              []                         = return ()
                                                         eqBranches ((l0,u0):lus0') ((l1,u1):lus1') | l0 == l1 = eq (u0,s0) (u1,s1) >> eqBranches lus0' lus1'
                                                         eqBranches _ _                                        = throwError "Case: branches differ"                                                          
    eq (NForce t) (NForce t') = eq t t'
    eq (NUnfold t xu) (NUnfold t' xu') = eq t t' >> eq xu xu'                                                            
    eq t u = throwError ("Different neutrals:\n"++ show t ++"\n/=\n"++ show u ++"\n")

instance Equal Boxed where
    eq (Boxed c) (Boxed c') = eqBox c c'
    
instance Equal Index where
    eq i0 i1
        | i0 == i1  = return ()
        | otherwise =  
                     do  t0@(u1,g1) <- do_env_lookup i0
                         t1@(u2,g2) <- do_env_lookup i1
                         case (u1,u2) of
                          (Undefined j0, Undefined j1) -> unless (j0 == j1) $ throwError "Different indices" 
                          (_, Undefined j1) -> error "eq Index Undefined 1"
                          (Undefined j0, _) -> error "eq Index Undefined 2"
                          _ -> letEval i0 (Undefined i0,emptyContext) (letEval i1 (Undefined i0,emptyContext) (eq t0 t1))
                                {- ^ Necessary e.g. to evaluate t12 in the following code:
                                    Eq : (a:Type) -> a -> a -> Type;
                                    Eq = \ a x y -> (P : a -> Type) -> P x -> P y;

                                    refl : (a:Type) -> (x:a) -> Eq a x x;
                                    refl = \ a x P px -> px;

                                    A : Type;
                                    a : A;

                                    t12 : Eq (^A) (let y:A=y in [y]) (let z:A=z in [z])
                                        = refl (^A)  (let y:A=y in [y]);
                                        -}

                           
{- eqBox implements alpha equality -}
eqBox :: Closure Term -> Closure Term -> Eval ()
eqBox c c' | c == c' = return ()
eqBox (Var l x,g) (Var l' y,g') =
    do 
    x' <- getContextIndex l  x g
    y' <- getContextIndex l' y g'
    eq x' y'
        
eqBox (Let _ p t,g) c =
    do g' <- evalProg (p,g)
       eqBox (t,g') c
eqBox c c'@(Let _ _ _,_) = eqBox c' c
eqBox (Q _ ps (a,(x,b)),g) (Q _ ps' (a',(x',b')),g')
      | ps == ps' =
          do eqBox (a,g) (a',g')
             eq (x,Boxed (b,g)) (x',Boxed (b',g'))
eqBox (Lambda _ (x,t),g) (Lambda _ (x',t'),g') =
      eq (x,Boxed (t,g)) (x',Boxed (t',g'))
eqBox (App t u,g) (App t' u',g') =
    do eqBox (t,g) (t',g')
       eqBox (u,g) (u',g')
eqBox (Pair _ t u,g) (Pair _ t' u',g') =
    do eqBox (t,g) (t',g')
       eqBox (u,g) (u',g')
eqBox (Split _ t (x,(y,u)),g) (Split _ t' (x',(y',u')),g') =
    do eqBox (t,g) (t',g')
       eq (x,(y,Boxed (u,g))) (x',(y',Boxed (u',g')))
eqBox (Case _ t bs,g) (Case _ t' bs',g') =
    do eqBox (t,g) (t',g')
       zipWithM_ (\ (l,t'') (l',t''') ->
                      if l==l' then eqBox (t'',g) (t''',g')
                      else fail "eqBox case") bs bs'
eqBox (Lift _ t,g)  (Lift _ t',g')  = eqBox (t,g) (t',g')
eqBox (Box _ t,g)   (Box _ t',g')   = eqBox (t,g) (t',g')
eqBox (Force _ t,g) (Force _ t',g') = eqBox (t,g) (t',g')
eqBox (Rec _ t,g)   (Rec _ t',g')   = eqBox (t,g) (t',g')
eqBox (Fold _ t,g)  (Fold _ t',g')  = eqBox (t,g) (t',g')
eqBox (Unfold _ t (x, u), g) (Unfold _ t' (x', u'), g') =
    do eqBox (t,g) (t',g')
       eq (x,Boxed (u,g)) (x',Boxed (u',g'))
eqBox (Undefined _,_) _ = error "eqBox Undefined not implemented 1"
eqBox _ (Undefined _,_) = error "eqBox Undefined not implemented 2"
eqBox (t,_) (t',_)  | t == t'   = return () -- Type, Label, Enum
                    | otherwise = error "Different terms"

-- * Evaluation with constraints
               
-- invariant: should never be called for inconsistent constraints.
ne :: Neutral -> Eval Value
ne n = do 
          Constraints cs <- ask
          r <- match cs n
          case r of
            Nothing -> return (VNeutral n)
            Just p  -> return (pat2val p) 
      where match :: [Constraint] -> Neutral -> Eval (Maybe Pattern)
            match [] _                      = return Nothing
            match (Constraint n' p : cs') n = ifOk (eq n n') (return (Just p)) (match cs' n)
                  
ifOk :: Eval a -> Eval r -> Eval r -> Eval r
ifOk c i e = do b <- catchError (c >> return True) (const $ return False)
                if b then i else e
                
-- * Main evaluation function

-- | Simulate substitution t [x <- u ].
-- | No actual substitution is performed.
-- | Rather, the closure u is added to the evaluation environment and a reference to u is added to the context g of term t.
subst :: Bind (Closure Term)  -> Closure Term -> Eval (Closure Term)
subst (x,(t,g)) u  = do
                     e <- getEnvironment
                     let i = size e
                     putEnvironment $ insert i (u,cUndefined i,PrintInfo x True) e
                     return $ (t,extendContext g x i)
                     
eval :: (Closure Term) -> Eval Value
eval (Let _ g' t, g)            = curry eval t =<< evalProg (g',g)
eval (Var l x, g)               = getContextIndex l x g >>= evalIndex
eval (Undefined i, _)           = ne $ NVar i 
eval (App t u, g)               = eval (t,g) >>= (`evalApp` (u,g)) 
eval (Case _ t lts, g)          = eval (t,g) >>= (`evalCase` (lts,g))
eval (Split _ t (x, (y, u)), g) = eval (t,g) >>= (`evalSplit` (x,(y,(u,g))))
eval (Force _ t, g)             = eval (t,g) >>= evalForce
eval (Unfold _ t (x, u), g)     = eval (t,g) >>= (`evalUnfold` (x, (u, g)))
eval t                          = return $ toValue t

toValue :: (Term, Context) -> Value
-- |  w -+-> w
toValue (Type _,_)              = VType
toValue (Q _ q c,g)             = VQ q (c,g)
toValue (Lambda _ (x,t), g)     = VLambda (x,(t,g)) 
toValue (Pair _  t u, g)        = VPair ((t,u),g)
toValue (Enum _ ls,_)           = VEnum ls
toValue (Label _ l,_)           = VLabel l
toValue (Lift _ t, g)           = VLift (t,g)
toValue (Box _ t, g)            = VBox (Boxed (t,g))
toValue (Rec _ t, g)            = VRec (t,g)
toValue (Fold _ t, g)           = VFold (t,g)

toValue t = error $ "toValue : missing pattern : " ++ show t
             
evalApp :: Value -> (Closure Term) -> Eval Value
evalApp (VLambda xt)  u = subst xt u >>= eval
                         
evalApp (VNeutral t)  u = ne $ NApp t u
evalApp _             _ = throwError "function expected"

evalCase :: Value -> ([(Label,Term)],Context) -> Eval Value
evalCase (VLabel l) (lts,g) =
    case lookup l lts of
      Nothing -> throwError "case not matched"
      Just t  -> eval (t,g)
evalCase (VNeutral n) cs  = ne $ NCase n cs
evalCase _            _   = throwError "Label expected"

evalSplit :: Value
          -> Bind (Bind (Closure Term))
          -> Eval Value
evalSplit (VPair ((l,r),g)) (x,(y,(t,g'))) =
    do ts'' <- subst (x, (t, g')) (l, g)
       eval =<< subst (y, ts'') (r, g)
evalSplit (VNeutral n) b     = ne $ NSplit n b
evalSplit  _           _     = throwError "Pair expected"

evalForce :: Value -> Eval Value
evalForce (VBox (Boxed c))   = eval c
evalForce (VNeutral n)       = ne $ NForce n
evalForce _                  = throwError "Box expected"

evalUnfold :: Value -> Bind (Closure Term) -> Eval Value
evalUnfold (VFold c)       b = eval =<< subst b c
evalUnfold (VNeutral n)    b = ne $ NUnfold n b
evalUnfold _               _ = throwError "Fold expected"

evalIndex :: Index -> Eval Value
evalIndex i = do_env_lookup i >>= eval

