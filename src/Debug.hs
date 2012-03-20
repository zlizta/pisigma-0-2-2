{-# LANGUAGE TypeSynonymInstances #-}
module Debug where

import Prelude 
import Data.List 
import Debug.Trace
import Data.Map (assocs)
import Control.Monad.Reader
import Text.PrettyPrint.ANSI.Leijen
  hiding ( Pretty)
import Location
import Syntax
import Environment
import Values
import Eval
import Pretty

{- Example of use: in the module Check, rename the check function check0 and re-define check as below.
module Check where
...
import Debug
...
check :: Closure Term -> Closure Type -> Eval ()
check  (t1, g1) (t2, g2)  =
  do
  traceA "check" [shD "t" t1, shD "g" g1,shD "T" t2,shD "G" g2] $
         check0 (t1, g1) (t2, g2)

check0 :: Closure Term -> Closure Type -> Eval ()
...
-}

-- debug = True
debug = False

traceA :: String -> [String] -> Eval b -> Eval b
traceA a ms c = if debug then  do
    traceEnv  (debmsgstart a)
    traceCons (debmsgstart a)
    traceC (map ((debmsg a)++) ms)
    r <- c
    traceEnv  (debmsgend a)
    traceCons (debmsgend a)
    traceC (map ((debmsg a)++) ms)     
    trace (take 100 $ repeat '-') $ return r
    else c
    
traceX s x = if debug then trace s x else x    
traceC ss = traceX (concat $ intersperse "\n" $ ss) (return ())


traceEnv a  = if debug then do 
           e <- getEnvironment
           trace (a++ showD e) return ()
           else return()

traceCons a = if debug then do
            cs <- ask
            trace (a ++ showD cs) return()
            else return ()
debmsg a = take 12 ("."++ a ++"." ++ repeat ' ') 
debmsgstart a =  take 12 (">"++ a ++"<" ++ repeat ' ')
debmsgend a   =  take 12 ("<"++ a ++">" ++ repeat ' ')

shD s x = (indent' 12 s ++ " = " ++ showD x)

indent' n s = take (n - length s) (repeat ' ') ++ s
             
class Debug a where
    showD :: a -> String           

instance Debug Label where
    showD = id

instance Debug Value where
    showD = showD . toTerm
                where
                toTerm (VNeutral n) = f n
                -- toTerm (VUndefined i) = Undefined i
                toTerm VType = Type Unknown 
                toTerm (VQ p (a,g)) = Q Unknown p a
                toTerm (VEnum ls) = Enum Unknown ls
                toTerm (VLift (t,_)) = Lift Unknown t
                toTerm  (VRec (t,_)) = Rec Unknown t
                
                toTerm t = error $ "234 toTerm (module Debug): " ++ show t
                 
                f (NVar i) = Var Unknown (":"++show i)
                f (NCase n (lts,_)) = Case Unknown (f n) lts 
                -- %%%f (NUndefined i)    = Var Unknown ("#"++ show i)
                f (NSplit n (x,(y,(t,_)))) = Split Unknown (f n) (x,(y,t))
                f (NUnfold n (x,(t,_))) = Unfold Unknown (f n) (x,t)
                f (NApp n (t,_)) = App (f n) t
                f (NForce n) = Force Unknown (f n)
                f t = error $ "235 toTerm, f (module Debug): " ++ show t
{-
data    Value = VNeutral Neutral
              -- | Weak head normal forms 
              | VUndefined Index
              | VType
              | VQ PiSigma  ((Type, Bind Type),Context)
              | VLambda     (Bind (Closure Term))
              | VPair       (Closure (Term, Term))
              | VEnum       [Label]
              | VLabel       Label
              | VLift       (Closure Type)
              | VBox         Boxed
              | VRec        (Closure Type)
              | VFold       (Closure Term)
              deriving (Eq, Show)
           
data  Neutral = NVar    Index
              | NUndefined Index
              | NApp    Neutral (Closure Term)
              | NCase   Neutral (Closure [(Label,Term)])
              | NSplit  Neutral (Bind (Bind (Closure Term)))
              | NForce  Neutral
              | NUnfold Neutral (Bind (Closure Term))
              deriving (Eq, Show)
-}
   
instance (Debug a, Debug b) => Debug (a,b) where
    showD (a,b) = showD a ++ "[" ++ showD b ++ "]"
instance Debug Environment where
    showD e = concat $  (:) (indent' 15 "ENVIRONMENT : ") $ intersperse " ;\n                           " $  h (assocs e)
            where
            f (i,e) = show i ++" "++g e
            g (t,a,_) = " " ++ showD (fst t) ++ "  :  " ++ showD(fst a) ++ "  < "  ++ showD (snd t) ++ ":" ++ showD (snd a) ++" >"
            h [] = ["empty"]
            h xs = map f xs
            
instance Debug Term where 
    showD = (concatMap f) . renderString . (prettyTerm 0)
                    where
                    f '\n' =  "\n" ++ (take 31 $ repeat ' ') 
                    f c = [c]
instance Debug Context where
        showD (Context []) = ""
        showD (Context s) = concat $ intersperse " ; " $ h s
                          where f (x,i) = show i ++ " <~ " ++ x 
                                h [] = ["empty"]
                                h xs = map f xs
instance Debug  Neutral where
    showD = showD . neutralToTerm

neutralToTerm :: Neutral -> Term
neutralToTerm (NVar i) = Var Unknown (show i)   
neutralToTerm (NApp   n (t,s)) = App (neutralToTerm n) t
neutralToTerm (NCase  n (lus,s)) = Case Unknown (neutralToTerm n) lus
neutralToTerm (NSplit n (x, (y, (t,s)))) = undefined
neutralToTerm (NForce n) = undefined
neutralToTerm (NUnfold n (x, (t,s)))     = undefined

instance Debug Pattern where
        showD (PLabel l) = l
        
instance Debug Constraint where
    showD (Constraint n p) = show n ++ " = " ++ showD p
    
instance Debug Constraints where 
    showD Inconsistent = indent' 15 "CONSTRAINTS : " ++ "INCONSISTENT"
    showD (Constraints cs) = concat $ (:) (indent' 15 "CONSTRAINTS : ") $ if null cs then ["none"]  else intersperse " , " $ map showD cs

  