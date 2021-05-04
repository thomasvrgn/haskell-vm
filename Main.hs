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
    print (parse content)
    let (ret, st) = runState (runBytecode [Push (String "test"), Store "a", Load "a", Load "a"]) emptyMemory
    printMem st