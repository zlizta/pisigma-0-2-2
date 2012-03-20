{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Environment where

import Prelude hiding (lookup)
import qualified Prelude as Prelude

import Data.Maybe (fromJust)
import Data.Map

import Location
import Syntax

type NOTHING = ()

type    Environment      = Map Index (Closure Term, Closure Type, PrintInfo)
type EnvironmentEntry    = (Closure Term, Closure Type)
data    PrintInfo        = PrintInfo { name :: Name , expand :: Bool }        deriving Show

newtype Context          = Context [(Name, Index)]    deriving (Show, Eq, Ord)                         
type    Closure a        = (a,Context)

class CLOSURE a where
  getContext :: a -> Context
  putContext :: a -> Context -> a

cUndefined i = (Undefined i ,emptyContext)

-- * Closures
instance GetLoc a => GetLoc (Closure a) where getLoc = getLoc . fst

-- * Contexts
emptyContext :: Context
emptyContext = Context []

-- | Add a new association (x,(i,m)) to context s. 
extendContext :: Context -> Name -> Index -> Context                        
extendContext (Context s) x i = Context $ (x,i):s


-- * Environments
emptyEnvironment = empty

env_lookup_term :: Index -> Environment -> Closure Term
env_lookup_term i e = case lookup i e of Just (t,_,_) -> t
                      
env_lookup_printInfo :: Index -> Environment -> PrintInfo
env_lookup_printInfo i e = 
                 case lookup i e of
                      Just (t,a,p) -> p

-- | Updates an existing entry and keeps the original printing info.
env_update_term :: Index -> Closure Term -> Environment -> Environment
env_update_term i t e = adjust (\(_,a,p) -> (t,a,p)) i e

-- * CLOSURES 
instance CLOSURE (Closure a) where
  getContext (_, s)   = s
  putContext (a, _) s = (a, s)

instance CLOSURE a => CLOSURE (Bind a) where
  getContext (_, a)   = getContext a
  putContext (x, a) s = (x, putContext a s)
  

