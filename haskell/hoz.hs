-- hoz.hs
--
-- One-time setup for students:
-- Before running for the first time, you must install the required libraries.
-- Open a terminal and run the following command:
--
--   cabal install --lib megaparsec mtl text
--
-- After this, you can load this file into ghci as usual.
--
-- Main executable that pipelines parser.hs -> elab.hs -> interp.hs
-- Imports: interp.hs, parser.hs, elab.hs (other modules for running the parser)

module Hoz where 

import Interp
import Parser
import Elab
import Kernel
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad
import Control.Monad.State.Strict
import Debug.Trace


-- Example execution
-- ghci> :load hoz.hs
-- hoz> runFull "declarative" "inFile.txt" "outFile.txt"
-- hoz> runFullT (Finite 3) "declarative" "inFile.txt" "outFile.txt"


-- choice to determine which statement parsers are being included
  -- Parsing option input:
  --"functional" -> funcStmts
  --"declarative" -> declStmts
  --"declarative threaded" -> declThreadStmts
  --"stateful" -> stateStmts
  --"stateful threaded" -> stateThreadStmts
  
  --  funcStmts = [pSkip, pBind, pLocal, pIf, pCase, pFunDef]
  --  declStmts = funcStmts ++ [pProcApp, pProcDef]
  --  declThreadStmts = declStmts ++ [pThread, pByNeed]
  --  stateStmts = declStmts ++ [pExchange, pNewCell, pSetCell]
  --  stateThreadStmts = stateStmts ++ [pThread, pByNeed]

-- Input: parsing option, input text file, output text file
-- Output: Parsing error Or 
--         Program ouput to terminal and elaborated kernel syntax to out file
runFull :: String -> String -> String -> IO ()
runFull opt inF outF = do 
  input <- readFile inF
  let (s,n) = runState (runParserT pStatementF "" input) opt -- set state with string opt
  case s of
    (Left bundle) -> putStr (errorBundlePretty bundle)
    (Right xs) -> do 
        let (elabData, _) = elab xs 0
        writeFile outF (showStmts elabData)
        case oz_p elabData of
          Left err -> putStrLn $ "Execution Error: " ++ err
          Right _ -> return ()
runFullT :: ExtInt -> String -> String -> String -> IO ()
runFullT q opt inF outF = do 
  input <- readFile inF
  let (s,n) = runState (runParserT pStatementF "" input) opt  -- set state with string opt
  case s of
    (Left bundle) -> putStr (errorBundlePretty bundle)
    (Right xs) -> do 
      let (elabData, _) = elab xs 0
      writeFile outF (showStmts elabData)
      case toz_p q elabData of
        Left err -> putStrLn $ "Execution Error: " ++ err
        Right _ -> return ()

-- Display errors for either non-threaded or threaded option for the interpreter
oz_p :: [Stmt] -> Either String ()
oz_p s = case oz s of
  SDone -> Right ()
  SError st -> Left st

toz_p :: ExtInt -> [Stmt] -> Either String ()
toz_p q s = case toz q s of
  SDone -> Right ()
  SError st -> Left st
