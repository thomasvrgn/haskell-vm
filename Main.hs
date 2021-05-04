{-# LANGUAGE BlockArguments #-}
module Main where
  import Core.Interpreter
  import Core.Parser

  import Types.Bytecode
  import Types.Stack
  import Types.Value

  import Control.Monad.State

  main :: IO()
  main = do
    content <- readFile "tests/Sample.hsc"
    let bytecode = parse content
    let (ret, st) = runState (runBytecode bytecode) emptyMemory
    printMem st
    printBytecode bytecode