{-# LANGUAGE OverloadedStrings #-}
module Error where

import Control.Monad.Error

import Location
import Syntax
import Environment
import Values
import Eval
import Evaluate
import Pretty

throwErrorc :: (Print b, GetLoc b) => b -> Doc -> Eval a
throwErrorc t m =
  do
    pt <- evalPrint t
    throwError $ renderString $
             text (locMessage $ getLoc t)
         <$> "Expression:   " <+> align pt
         <$> m
expectedButFound :: (Print b1, Print a, Print a1, GetLoc b1) =>
     b1 -> a -> a1 -> String -> Eval b        
expectedButFound t expected' found inferred =
  do
    pExpected <- evalPrint expected'
    pFound    <- evalPrint found
    throwErrorc t $
          "Inferred type:" <+> align pFound
      <$> "Expected type:" <+> align pExpected
      <$> parens (text inferred)
      
expected :: Closure Term -> Value -> Doc -> Eval ()
expected t expected' inferred = do
  pExpected <- evalPrint expected'
  throwErrorc t $ "Expected type:" <+> align pExpected <$> parens inferred