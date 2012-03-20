module Test where

import Prelude  hiding (putStrLn)

import Control.Monad.State
import PSIO
import REPL

import Environment
import Main (start)

test :: IO ()
test = 
    do
    start
    let ini :: REPL ()
        ini = do
              mapM_ (\x -> interpretInput (Just x) >>= handleCommand) 
                     [ ":l Vec.pi"
                     , ":l Nat.pi"
                     , ":l Streams.pi"
                     , ":l Equal.pi"
                     , ":l Universe.pi"
                     , ":l Curry.pi"
                     , ":l Parser.pi"
                     , ":l Conat.pi"
                     , ":l Hurkens.pi"
                     , ":l Bintree.pi"
                     , ":l Id.pi"
                     , ":l LackOfSubjectReduction.pi"
                     , ":l LackOfSubjectReductionCase.pi"
                     , ":l Maybe.pi"
                     , ":l Parser.pi"
                     , ":l shin.pi"
                     , ":l stl.pi"
                     ]
              (Context sc, _) <- gets replState
              mapM_ (\x -> (replMessage  $ ":::  " ++  x) >> interpretInput (Just x) >>= handleCommand) 
                     (filter (not . (`elem` ["loop", "contradiction", "id''"])) $ map fst sc)
    runREPL (handleCommand Startup >> ini >> repl)   
    putStrLn ""