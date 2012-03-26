{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pretty
  (   Doc
    , renderString,renderIO
    , text
    , align
    , (<+>)
    , (<$>)
    , parens
    , Print
    , vcat
    , colon
    , evalPrint
    , prettyTerm
    , prettyProg)
  where

import Text.PrettyPrint.ANSI.Leijen
  hiding (Pretty)
import System.IO (stdout)
import Data.Char

import Evaluate
import Normalise
import Syntax
import Eval
import Values
import Environment
import PSIO
import Symbols


renderString :: Doc -> String
renderString doc = displayS (renderPretty defaultRibbon defaultPageWidth doc) ""

renderIO :: Doc -> IO ()
renderIO doc = displayIO stdout $ renderPretty defaultRibbon defaultPageWidth doc

reserved = dullblue . bold . text
prettyArrow = text symbolArrow              
prettyStar  = text "*"
prettyLambdaSymbol = text symbolLambda
prettyUpArrowSymbol = text "^"
prettyExclamationSymbol = "!"

prettyWordLet                 = reserved "let"
prettyWrodIn                  = reserved "in"
prettyWordType                = reserved "Type"
prettyPiArrow                 = prettyArrow
prettySigmaStar               = prettyStar

              
prettyLambda                  = prettyLambdaSymbol
prettyLambdaArrow             = prettyArrow
prettyWordSplit               = reserved "split"
prettyWordWith                = reserved "with"
prettySplitArrow              = prettyArrow
prettyWordCase                = reserved "case"
prettyWordOf                  = reserved "of"
prettyLiftOp                  = dullgreen prettyUpArrowSymbol
prettyForceOp                 = dullgreen prettyExclamationSymbol
prettyBoxLeftBracket          = dullgreen $ text "["
prettyBoxRightBracket         = dullgreen $ text "]"
prettyWordRec                 = reserved "Rec"
prettyWordFold                = reserved "fold"
prettyWordUnfold              = reserved "unfold"
prettyWordAS                  = reserved "as"
prettyUnfoldArrow             = prettyArrow

prettyProgDefEqual            = text "="
prettyProgDeclColon           = text ":"
prettyProgSemiColon           = text ";"

prettyBranchOpen              = text "{"
prettyBranchClose             = text "}"
prettyBranchSep               = text "|"
prettyBranchArrow             = dullred $ prettyArrow
prettyLabel l                 = dullred $ text l
prettyEnumLabel               = prettyLabel


-- * Print class for various types

class Print a where
    evalPrint :: a -> Eval Doc

instance Print String where
    evalPrint   = return . text

instance Print Doc where
    evalPrint   = return

instance Print Value where
    evalPrint a = evalPrint =<< quote [] a

instance Print (Closure Term) where
     evalPrint a = evalPrint =<< quote [] a -- bug1 : might print the name of a variable no longer in scope
                                            -- see also Normalize.hs :      qq xs (Var l x  , s) = return $ Var l x
    
instance Print Neutral where
    evalPrint a = evalPrint =<< quote [] a

instance Print Term where
    evalPrint   = return . prettyTerm 0



data Msg
  = MsgV Value
  | MsgC (Closure Term)
  | MsgT Term
  | MsgS String
  | MsgD Doc

instance Print Msg where
    evalPrint (MsgV v) = evalPrint v
    evalPrint (MsgC c) = evalPrint c
    evalPrint (MsgT t) = evalPrint t
    evalPrint (MsgS s) = evalPrint s
    evalPrint (MsgD d) = evalPrint d

-- * Pretty printer for terms

-- | Context for printing. Determines whether parentheses are
-- required.
type PrintContext = Int


prettyTerm :: PrintContext -> Term -> Doc

prettyTerm _ (Undefined i)               =
      reserved $ "#" ++ show i
      
prettyTerm _ (Var _ x)                   =
      prettyVar x

prettyTerm c (Let _ p t)                 =
      contextParens c 0
   $  prettyWordLet
  <+> sep (punctuate prettyProgSemiColon $ map prettyEntry p)
  <+> prettyWrodIn
  <+> prettyTerm 0 t

prettyTerm _ (Type _)                    =
      prettyWordType

prettyTerm c (Q _ Pi (t1, (n, t2)))      =
      contextParens c 0
   $  group
   $  binding 1 n t1
  <$> prettyPiArrow
  <+> prettyTerm 0 t2

prettyTerm c (Q _ Sigma (t1, (n, t2))) =
      contextParens c 1
   $  binding 2 n t1
  <+> prettySigmaStar
  <+> prettyTerm 1 t2

prettyTerm c (Lambda _ (n, t))              =
      contextParens c 0
   $  prettyLambda
  <+> prettyVar n
  <+> accumLambda t
  where
  accumLambda (Lambda _ (n,t)) = prettyVar n <+> accumLambda t
  accumLambda t = prettyLambdaArrow <+> prettyTerm 0 t

prettyTerm c (App t1 t2)                 =
      group
   $  hang 2
   $  contextParens c 2
   $  prettyTerm 2 t1
  <$> prettyTerm 3 t2

  
prettyTerm _ (Pair _ t1 t2)              =
      tupled $ map (prettyTerm 0) [t1, t2]

prettyTerm c (Split _ t1 (n1, (n2, t2))) =
      contextParens c 0
   $  hang 2
   $  prettyWordSplit
  <+> prettyTerm 0 t1
  <+> prettyWordWith
  <+> (parens $  prettyVar n1
              <> comma
              <> prettyVar n2)
  <+> prettySplitArrow
  <$> prettyTerm 0 t2

prettyTerm _ (Enum _ ls)                 =
      braces
   $  align
   $  fillSep
   $  map prettyEnumLabel ls

prettyTerm _ (Label _ l)                 =
      text ('\'':l)

prettyTerm _ (Case _ t bs)               =
      hang 1
   $  prettyWordCase
  <+> prettyTerm 0 t
  <+> prettyWordOf
  <$> branches bs

prettyTerm c (Lift _ t)                  =
      contextParens c 2
   $  prettyLiftOp
  <+> prettyTerm 3 t

prettyTerm _ (Box _ t)                   =
      prettyBoxLeftBracket <+> prettyTerm 0 t <+> prettyBoxRightBracket

prettyTerm c (Force _ t)                 =
      contextParens c 2
   $  prettyForceOp
  <+> prettyTerm 3 t

prettyTerm c (Rec _ t)                  =
     contextParens c 2
   $ prettyWordRec
  <+> prettyTerm 3 t

prettyTerm c (Fold _ t)                  =
     contextParens c 2
   $ prettyWordFold
  <+> prettyTerm 3 t

prettyTerm c (Unfold _ (Var _ x) (n, Var _ n')) | n == n' =
      contextParens c 2
   $  prettyWordUnfold
  <+> prettyVar x

prettyTerm c (Unfold _ t1 (n, t2))       =
      contextParens c 0
   $  hang 2
   $  prettyWordUnfold
  <+> prettyTerm 0 t1
  <+> prettyWordAS
  <+> prettyVar n
  <+> prettyUnfoldArrow
  <$> prettyTerm 0 t2

prettyProg :: Prog -> Doc
prettyProg = cat . (map prettyEntry)

prettyEntry :: ProgramEntry -> Doc

prettyEntry (Defn _ n t)                 =
      hang 2
   $  prettyVar n
  <+> prettyProgDefEqual
  <+>  prettyTerm 0 t

prettyEntry (Decl _ n t)                 =
      hang 2
   $  prettyVar n
  <+> prettyProgDeclColon
  <>  prettyTerm 0 t


binding :: PrintContext -> Name -> Term -> Doc
binding c n t | null n =  prettyTerm c t
              | otherwise       =  parens
                                $  prettyVar n
                               <+> colon
                               <+> prettyTerm 0 t

branches :: [(Label,Term)] -> Doc
branches ls = encloseSep prettyBranchOpen prettyBranchClose prettyBranchSep (map branch ls)
  where
    branch (l, t) =  fillBreak k (prettyLabel l)
                 <+> prettyBranchArrow
                 <+> align (prettyTerm 0 t)
    k = maximum $ map (length.fst) ls
    
-- | Prints parens if the current context is higher
-- than a certain limit.
contextParens :: PrintContext -> PrintContext -> Doc -> Doc
contextParens c d p | c > d     = parens p
                    | otherwise = p