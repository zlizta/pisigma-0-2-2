module Constraints where

import Control.Monad.Reader
import Control.Monad.Error

import Syntax
import Environment
import Values
import Eval
import Evaluate

consistent :: Eval Bool
consistent = do cs <- ask
                case cs of
                  Inconsistent -> return False
                  Constraints _ -> return True

-- Add the constraint term = pattern to the evaluation constraints.
addContraint :: Closure Term -> Pattern -> Eval a -> Eval a
addContraint closureTerm@(term,context) pattern constraint =
        -- traceA "addContraint" [shD "term" term,shD "G" context, shD "pattern" pattern] $
        addContraint_0 closureTerm pattern constraint

addContraint_0 t p k = do
  cs <- ask
  case cs of
    Inconsistent    -> k
    Constraints cs' -> do v <- eval t
                          case v of
                            VLabel l ->
                                       case p of
                                        PLabel l' | l == l'   -> k
                                                  | otherwise -> local (const Inconsistent) $ k
                            VNeutral n -> local (const (Constraints (Constraint n p : cs'))) $ k
                            -- should check for mutual inconsistency!

