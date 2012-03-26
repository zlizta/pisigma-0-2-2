{-# LANGUAGE    StandaloneDeriving         #-}
{-# LANGUAGE    GeneralizedNewtypeDeriving #-}
{-# LANGUAGE    MultiParamTypeClasses      #-}
{-# LANGUAGE    ScopedTypeVariables        #-}

module REPL where

import Prelude hiding (catch,readFile, putStrLn)
import qualified Data.List as List
import Data.Char
import System.Console.Haskeline.Class
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans
import GHC.Read
import Text.PrettyPrint.ANSI.Leijen
  hiding (Pretty)
import System.IO (stdout)
  
import PSIO 
import Error
import Check
import Equal
import Environment
import Eval
import Evaluate
import Lexer as Lexer ( Parser )
import Parser
import Pretty
import Syntax
import Normalise
import Version
import Symbols



-- TODO: better command line arg handling; allow non-interactive mode

-- * Instances needed for newtype deriving

instance (Error e, MonadException m) => MonadException (ErrorT e m) where
  catch m = ErrorT . catch (runErrorT m) . (runErrorT .)
  block   = mapErrorT block
  unblock = mapErrorT unblock

instance (Error e, MonadHaskeline m) => MonadHaskeline (ErrorT e m) where
  getInputLine = lift . getInputLine
  getInputChar = lift . getInputChar
  outputStr    = lift . outputStr
  outputStrLn  = lift . outputStrLn

-- * Types

-- | The REPL monad has exceptions, a line editor with history using
-- Haskeline, and state.
newtype REPL a = REPL { unREPL :: ErrorT REPLError (HaskelineT (StateT REPLState IO)) a }
  deriving ( Monad
           , MonadException
           , MonadError REPLError
           , MonadHaskeline
           , MonadIO
           , MonadState REPLState )

-- | The state currently keeps track of identifiers defined and their
-- types, as well as a list of files currently loaded.
data REPLState =
  REPLState
    { replState  :: (Context, Environment)
    , replFiles  :: [FilePath]
    , replRibbon :: Float       -- ^ ribbon width as a ratio of page width. The ribbon width is the maximal amount of non-indentation characters on a line.
    , replWidth  :: Int         -- ^ page width for prettyPrinting
    }

-- TODO: Add a search path and verbosity options.

-- ** Commands

-- | Commands that can be entered at the REPL prompt.
data REPLCommand =
    Load String
  | Reload
  | Quit
  | EvalPhrase Phrase
  | Noop
  | Clear
  | Equal (Term,Term)
  | TypeOf Term
  | Help
  | Browse
  | Startup
  | Version
  | Width String
  | Ribbon String
  | ShowSymbols

-- ** Errors

-- | Normal Errors abort the current command but do not quit the REPL.
-- If we actually wish to quit, we can throw a Terminate error.
data REPLError =
    Error
  | Terminate

instance Error REPLError where
  noMsg    = Error
  strMsg _ = Error

render :: Doc -> REPL()
render doc = do 
             w <- gets replWidth
             r <- gets replRibbon
             liftIO $ displayIO stdout $ renderPretty r w doc


-- * REPL monad
-- (Read Evaluate Print Loop --- terminology used in the Haskeline module.)
initialREPLState :: REPLState
initialREPLState =
  REPLState 
      { replState  = (emptyContext, emptyEnvironment) 
      , replFiles  = []
      , replRibbon = defaultRibbon        
      , replWidth  = defaultPageWidth
      }
runREPL :: REPL () -> IO ()
runREPL r =
  do
    runStateT (runHaskelineT (setComplete pisigmaCompletion defaultSettings)
                         (runErrorT (unREPL r)))
              initialREPLState
    return ()

-- * Haskeline

-- | Completion in PiSigma. For the moment, we use file name completion
-- while in a load command, and identifier completion everywhere else.
pisigmaCompletion :: CompletionFunc (StateT REPLState IO)
pisigmaCompletion (x1, x2)
  | ":l" `List.isPrefixOf` List.reverse x1 = completeFilename (x1, x2)
  | otherwise                              = completeWord Nothing " " identifier (x1, x2)
  where
    identifier :: String -> StateT REPLState IO [Completion]
    identifier x = do
      (Context sc, _) <- gets replState
      let names = map fst sc
          cands = filter (x `List.isPrefixOf`) names
      return $ map simpleCompletion (List.sort cands)


-- * Help

-- | Preliminary interpreter help message.
help :: String
help = List.unlines
  [ "PiSigma currently supports the following commands:"
  , ""
  , "  :l         load a source file"
  , "  :r         reload current source file"
  , "  :b         browse current identifiers"
  , "  :c         clear the environment"
  , "  :t         ask for the type of a term"
  , "  :e         test two terms for beta equality"
  , "  :v         version"
  , "  :q         quit"
  , ""
  , "Type a declaration or an expression to evaluate it."
  , ""
  , "* Display options"
  , ""
  , "  :dr        define the ribbon width"
  , "  :dw        define the page width"
  ]

-- * REPL core

-- | Performs a single REPL step. Print prompt, read input,
-- dispatch.
replStep :: REPL ()
replStep =
  do
    f <- gets replFiles
    x <- getInputLine (List.unwords (List.reverse f) ++ "> ")
    c <- interpretInput (fmap id x)
    handleCommand c

-- | Run the interpreter as long as desired.
repl :: REPL ()
repl =
  do
    catchError replStep
      (\ e -> case e of
                Error      ->  return ()
                Terminate  ->  throwError Terminate)
    repl

-- * Command handling

-- | Preliminary input interpretation, based on the
-- current parser and no particular intelligence in
-- parsing commands correctly.
interpretInput :: Maybe String -> REPL REPLCommand
interpretInput Nothing  = return Quit
interpretInput (Just x)
  | ":dw" `List.isPrefixOf` x = return $ Width $ trim xArg
  | ":dr" `List.isPrefixOf` x = return $ Ribbon $ trim xArg
  | ":ss" `List.isPrefixOf` x = return $ ShowSymbols
  | ":l"  `List.isPrefixOf` x = return $ Load $ norm $ trim xArg
  | ":r"  `List.isPrefixOf` x = return Reload
  | ":b"  `List.isPrefixOf` x = return Browse
  | ":q"  `List.isPrefixOf` x = return Quit
  | ":c"  `List.isPrefixOf` x = return Clear
  | ":e"  `List.isPrefixOf` x = parseInputInteractive s2Terms Equal  xArg
  | ":t"  `List.isPrefixOf` x = parseInputInteractive sTerm   TypeOf xArg
  | ":h"  `List.isPrefixOf` x
  || ":?" `List.isPrefixOf` x = return Help
  | ":v"  `List.isPrefixOf` x = return Version
  | ":"   `List.isPrefixOf` x = replMessage "unknown command" >> return Noop
  | otherwise                 = parseInputInteractive sPhrase EvalPhrase x
  where
    norm  :: String -> String
    norm []              = []
    norm ('\\' : c : xs) = c : norm xs
    norm (       c : xs) = c : norm xs

    trim :: String -> String
    trim                = f. f where f = List.reverse . List.dropWhile isSpace

    xArg  :: String
    xArg                 = snd $ break (== ' ') x

-- | Turn a string into a REPL command.
parseInput :: FilePath
           -> Parser t
           -> (t -> REPLCommand)
           -> String
           -> REPL REPLCommand
parseInput f p cmd s =
    case parse p f s of
      Left err   -> do
        replMessage $ "Parse error: " ++ (show) err ++ "\n"
        return Noop
      Right p'   -> return (cmd p')

parseInputInteractive :: Parser t
                      -> (t -> REPLCommand)
                      -> String
                      -> REPL REPLCommand
parseInputInteractive p cmd = parseInput "<interactive>" p cmd

-- | Command handler. Returns a flag indicating whether
-- the interpreter should continue.
handleCommand :: REPLCommand -> REPL ()
handleCommand c =
  case c of
    Version-> replMessage $ "PiSigma version " ++ showVersion version
    Startup-> handleCommand Version >> replMessage "Type :? for help."
    Help   -> replMessage help
    Load f ->
      do
        fs <- gets replFiles
        if f `elem` fs then
            replMessage $ "Skipping " ++ f ++ "."
          else do
            mx <- liftIO $ liftM (Just) (readFile f)
                    `catch` (\ (_ :: IOException) -> return Nothing)
            case mx of
              Nothing -> do
                replMessage $ "Could not find " ++ f ++ "."
                throwError Error
              Just x -> do
                -- Allow source files to have interpreter commands at the top;
                -- we currently use this as a replacement for a module system
                let spanP s      = case s of
                                     ':':_ -> True
                                     _     -> False
                    (cmds, rest) = List.span spanP $ lines x
                mapM_ (\ c' -> handleCommand =<< (interpretInput . Just) c') cmds
                p <- parseInput f sProg (EvalPhrase . Prog)
                                (List.unlines (List.replicate (List.length cmds) "" ++ rest))
                handleCommand p
                modify (\ s -> s { replFiles =
                                            case replFiles s of
                                             (g : fs') | g == f -> g : fs'
                                             xs                 -> f : xs })
                -- We print the "Loaded" message after executing initial
                -- interpreter commands in order to reflect the dependency
                -- order of different source files.
                replMessage $ "Loaded " ++ f ++ "."
    Reload ->
      do
        f <- gets replFiles
        handleCommand Clear
        mapM_ (handleCommand . Load) (List.reverse f)
    Browse ->
      do
        (Context sc, _) <- gets replState
        let names :: [Doc]
            names = map (text . fst) sc
        render $ vcat names
        liftIO $ putStrLn ""
    Quit ->
      throwError Terminate
    EvalPhrase (Prog p) ->
      execProg p
    EvalPhrase (Term t) ->
      execTerm t
    Equal (t1,t2) ->
      eqTerms t1 t2
    TypeOf t ->
      inferTerm t
    Clear ->
      put initialREPLState
    Noop ->
      return ()
    Width w -> do 
               modify (\s -> s {replWidth = read w :: Int})
    Ribbon r -> do 
               modify (\s -> s {replRibbon = read r :: Float})
    ShowSymbols -> symbolSearch
-- ** Command handler helpers

execProg :: Prog -> REPL ()
execProg p =
  do
    s <- get
    let (con,env) = replState s
    case run env (checkProg (p,con)) of
      Right s' -> put (s { replState = s' })     
      Left e   -> do
                    liftIO $ putStrLn e
                    throwError Error

execTerm :: Term -> REPL ()
execTerm t =
  do
    s <- get
    let (con,env) = replState s
        p = do
          a  <- infer (t,con)
          t' <- nf [] (t,con)
          pa <- evalPrint a
          pt <- evalPrint t'
          return $ pt <$> colon <+> align pa
    case run env p of
        Right (m,_) -> render (m <> line)
        Left e      -> do
                         liftIO $ putStrLn e
                         throwError Error

eqTerms :: Term -> Term -> REPL ()
eqTerms t1 t2 =
  do
    s <- get
    let (con,env) = replState s
        p = eq (t1,con) (t2,con)
    case run env p of
        Right _ -> liftIO $ putStrLn "yes"
        Left e  -> do
                     liftIO $ putStrLn e
                     throwError Error


inferTerm :: Term -> REPL ()
inferTerm t =
  do
    s <- get
    let (con,env) = replState s
        p = evalPrint =<< infer (t,con)
    case run env p of
        Right (m,_) -> do
          render m
          liftIO $ putStrLn ""
        Left e      -> do
                         render $  text e
                         throwError Error

symbolSearch :: REPL ()
symbolSearch =  liftIO $ putStrLn $ symbolsList

-- | Placeholder for a message function that can depend
-- on verbosity settings.
replMessage :: String -> REPL ()
replMessage m = liftIO . putStrLn $  m
