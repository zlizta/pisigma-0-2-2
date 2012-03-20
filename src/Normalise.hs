{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module Normalise
  ( quote
  , nf )
  where

import Control.Monad

import Location
import Syntax
import Environment
import Values
import Eval
import Evaluate (eval)

{- Implementation of a normalisation function.
   Used for pretty printing and testing.
-}

type Vars = [Name]

fresh :: Name -> Vars -> Name
fresh x xs
  | x /= "" && x `elem` xs = (x ++ "'") `fresh` xs
  | otherwise              = x

class Nf a b | a -> b where
    nf :: Vars -> a -> Eval b
    nf = nf' True
    quote :: Vars -> a -> Eval b
    quote = nf' False
    nf' :: Bool -> Vars -> a -> Eval b

instance Nf (Closure Term) Term where
    nf' True  xs t = nf' True xs =<< eval t
    nf' False xs t = qq xs t

instance Nf Index Term where
    nf' b xs i = do e <- getEnvironment
                    let (PrintInfo x shouldExpand) = env_lookup_printInfo i e
                    case env_lookup_term i e of
                      (Undefined i,_) -> return (Var Unknown x)
                      t -> if shouldExpand then 
                                         do t' <- nf' b xs t
                                            return (Let Unknown 
                                                [Defn Unknown x t'] (Var Unknown x))
                                        -- We should also declare x, but we don't know its type.
                                        -- A let cannot be expanded if inside a box.
                                     else return (Var Unknown x)

qq :: Vars -> Closure Term -> Eval Term
qq xs (Var l x  , s) = return $ Var l x
qq xs (Let l g t, s) = 
             do s' <- evalProg (g,s)
                let prog_declared_vars prg = concat [case p of (Decl _ x _) -> [x]
                                                               _            -> []
                                                    | p <- prg ]
                qq (xs ++ prog_declared_vars g) (t,s')
                
                
qq xs (Q l ps (a,(x,b)),s) =
    do a' <- qq xs (a,s)
       xb' <- quote xs (x,(b,s))
       return (Q l ps (a',xb'))
qq xs (Lift l t,s) = liftM (Lift l) (qq xs (t,s))
qq xs (Rec l t,s) = liftM (Rec l) (qq xs (t,s))
qq xs (Fold l t,s) = liftM (Fold l) (qq xs (t,s))
qq xs (Unfold l t (x, u), s) = do t' <- qq xs (t, s)
                                  xu' <- quote xs (x, (u, s))
                                  return (Unfold l t' xu')
qq xs (Lambda l (x,t), s) = liftM (Lambda l) (quote xs (x,(t,s)))
qq xs (App t u ,s) = do t' <- qq xs (t,s)
                        u' <- qq xs (u,s)
                        return (App t' u')
qq xs (Pair l t u,s) = do t' <- qq xs (t,s)
                          u' <- qq xs (u,s)
                          return (Pair l t' u')
qq xs (Split l t (x,(y,u)),s) = do t' <- qq xs (t,s)
                                   xyu' <- quote xs (x,(y,(u,s)))
                                   return (Split l t' xyu')
qq xs (Case l t lts,s) = do t' <- qq xs (t,s)
                            lts' <- mapM (\ (l',t'') ->
                                              do t''' <- qq xs (t'',s)
                                                 return (l',t''')) lts
                            return (Case l t' lts')
qq xs (Box l t,s) = liftM (Box l) (qq xs (t,s))
qq xs (Force l t,s) = liftM (Force l) (qq xs (t,s))
qq xs (Undefined _,_) = error "qq xs Undefined not yet defined"
qq _ (t,_) = return t -- Type, Enum, Label

instance (CLOSURE a,Nf a b) => Nf (Bind a) (Bind b) where
    nf' b xs (x,t)  = do let x' = fresh x xs
                         (_,s') <- do_env_declare x (PrintInfo x' True)( getContext t) cUndefined
                         t' <- nf' b (x':xs) (putContext t s')
                         return (x',t')

instance Nf Value Term where
    nf' b xs (VNeutral n) = nf' b xs n
    nf' _ _  VType = return (Type Unknown)
    nf' b xs (VQ ps ((a,(x,c)),s)) = do a' <- nf' b xs (a,s)
                                        xc' <- nf' b xs (x,(c,s))
                                        return (Q Unknown ps (a',xc'))
    nf' b xs (VLift c) = liftM (Lift Unknown) (nf' b xs c)
    nf' b xs (VRec c) = liftM (Rec Unknown) (nf' b xs c)
    nf' b xs (VFold c) = liftM (Fold Unknown) (nf' b xs c)
    nf' b xs (VLambda xt) = liftM (Lambda Unknown) (nf' b xs xt)
    nf' b xs (VPair ((t,u),s)) = do t' <- nf' b xs (t,s)
                                    u' <- nf' b xs (u,s)
                                    return (Pair Unknown t' u')
    nf' _ xs (VBox (Boxed c)) = liftM (Box Unknown) (nf' False xs c)
    nf' _ _  (VEnum ls) = return (Enum Unknown ls)
    nf' _ _  (VLabel l) = return (Label Unknown l)

instance Nf Neutral Term where
    -- %%% nf' b xs (NUndefined i) = nf' b xs i
    nf' b xs (NVar i) = nf' b xs i
    nf' b xs (NApp t u) = do t' <- nf' b xs t
                             u' <- nf' b xs u
                             return (App t' u')
    nf' b xs (NSplit t xyu) = do t' <- nf' b xs t
                                 xyu' <- nf' b xs xyu
                                 return (Split Unknown t' xyu')
    nf' b xs (NCase t (lus,s)) = do t' <- nf xs t
                                    lus' <- mapM (\ (l,u) ->
                                                   do u' <- nf' b xs (u,s)
                                                      return (l,u')) lus
                                    return (Case Unknown t' lus')
    nf' _ xs (NForce t) = liftM (Force Unknown) (nf xs t)
    nf' b xs (NUnfold t xu) = do t' <- nf' b xs t
                                 xu' <- nf' b xs xu
                                 return (Unfold Unknown t' xu')
