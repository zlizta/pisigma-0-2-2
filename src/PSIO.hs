{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module PSIO where

import qualified System.IO      as IO
import qualified System.IO.UTF8 as IOUTF8
import Text.PrettyPrint.ANSI.Leijen
  hiding ( Pretty)
import Data.Char (ord)
import Symbols

-- Definitions that are OS dependent

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
readFile = IOUTF8.readFile
writeFile = IOUTF8.writeFile
putStrLn = IOUTF8.putStrLn
defaultPageWidth = 150 :: Int
defaultRibbon    = 0.6 :: Float
symbolArrow = "->" 
symbolLambda = "\\"
prettyVar x                   = doc
                                where 
                                doc   = if b then underline (dullmagenta (text s)) else text x
                                (s,b) = conv x
                                conv []                               = ("",False)
                                conv (c:cs) | ord c <128 = let (ds,b) = conv cs in (c:ds,b) 
                                            | otherwise  = let (ds,b) = conv cs
                                                               s = head $ [ s ++ (if i>0 then show i else "") | (s,xs@((_,i):_))<- map (\(s,cs)->(s,filter ((==c).fst) (zip cs [0..]))) symbolsShortcuts, not (null xs)] 
                                                                          ++ ["?"]
                                                            in (s++ds,True)
#else
readFile         = IO.readFile
writeFile        = IO.writeFile
putStrLn         = IO.putStrLn
defaultPageWidth = 150 :: Int
defaultRibbon    = 0.5 :: Float
symbolArrow      = "->" -- "¡ú"
symbolLambda     = "\\" -- "¦Ë"
prettyVar        = text
#endif

