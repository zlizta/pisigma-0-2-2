
{- PiSigma

@article{alti2010flops},
  author    = {Thorsten Altenkirch and Nils Anders Danielsson and Andres L\"oh and Nicolas Oury},
  title     = {{$\Pi$}{$\Sigma$}: Dependent Types Without the Sugar},
  journal   = {Functional and Logic Programming},
  pages     = {40--55},
  year      = {2010},
  publisher = {Springer}
-}

module Main where

import System.Environment
import System.Info
import Version
import Prelude  hiding (putStrLn)
import qualified System.IO.UTF8 as IOUTF8

import PSIO
import REPL
import Parser  
import Pretty
import Symbols
main = 
    do   
    start
    args <- getArgs
    let ini = mapM_ (handleCommand . Load) args
    runREPL (handleCommand Startup >> ini >> repl)
    
    

start = 
    do          
    progName <- getProgName
    putStrLn progName
    putStrLn $ unlines $ [dashline
                         ,"OS:              " ++ os
                         ,"Architecture:    " ++ arch
                         ,"Compiled with:   " ++ compilerName
                         ,"                 version " ++ showVersion compilerVersion
                         ,"Call name:       " ++ progName] 
    -- displayTestStrings -- For testing display of Unicode characters.
    putStrLn dashline 
    where
    dashline = take defaultPageWidth $ repeat '-'
    
-- Displays strings of letters and symbols to check whether characters are displayed properly:
displayTestStrings = 
   do
   catch (renderIO $ text $ (concatMap (\s -> "Characters test: " ++ s ++ "\n") strings))
         (\e -> do let err = show e
                   putStrLn ("\nWarning: Could not print the test string.\nThe error was:\n" ++ err ++ "\nCharacters out of the code page might not be displayed properly.")
                   return ())
   where     
   strings = splitsAt (defaultPageWidth - 20) $ "abc...xyz01234567890=" ++ symbolsList 
   splitsAt i [] = []
   splitsAt i xs = let (z,ys) = splitAt i xs in z : splitsAt i ys

-- In GHCi, enter   ps foo.pi   to launch the PiSigma interpreter with file foo.pi 
ps :: String -> IO ()
ps arg =
    do
    start
    let ini :: REPL () 
        ini = handleCommand $ Load arg
    runREPL (handleCommand Startup >> ini >> repl)
    

    
