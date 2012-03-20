{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Equal where

import Control.Monad.Error

import Syntax
import Environment
import Values
import Eval

class Equal a where
    eq :: a -> a -> Eval ()
    
eqBind :: (CLOSURE a) => (a -> a -> Eval ()) -> Bind a -> Bind a -> Eval ()       
eqBind eq (x0,c0) (x1,c1) =
  do let g0 = getContext c0
         g1 = getContext c1
     (i,g0') <- do_env_declare x0 (PrintInfo x0 True) g0 cUndefined
     let g1' = extendContext g1 x1 i
     let c0' = putContext c0 g0'
         c1' = putContext c1 g1'
     eq c0' c1'

instance (Equal a,CLOSURE a) => Equal (Bind a) where
    eq = eqBind eq
    

    
