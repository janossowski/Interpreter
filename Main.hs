module Main where

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

import AbsSmol
import LexSmol ( Token, mkPosToken )
import ParSmol ( pProgram, myLexer )
import PrintSmol ( Print, printTree )

import Interpreter
import System.IO (hPutStrLn, stderr)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = readFile f >>= run v

run :: Verbosity -> String -> IO ()
run v s =
  case pProgram ts of
    Left err -> do
      putStrLn "\nParse Failed...\n"
      putStrLn err
      exitFailure
    Right tree -> do
      interpretProgram tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> getContents >>= run 2
    "-s":fs    -> mapM_ (runFile 0) fs
    fs         -> mapM_ (runFile 2) fs

interpretProgram :: Program -> IO ()
interpretProgram program = do
  let initialState = (Map.empty, Store Map.empty Map.empty Map.empty Map.empty)
  result <- runStateT (runExceptT $ executeProgram program) initialState
  case result of
    (Left err, _) -> hPutStrLn stderr $ "Execution failed: " ++ show err
    _ -> return ()
