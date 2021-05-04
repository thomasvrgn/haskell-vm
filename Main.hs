{-# LANGUAGE BlockArguments #-}
module Main where
  import Types.Value
  import Types.Stack
  import Types.Bytecode
  import Control.Monad.State
  
  stackTests = do
    (stack, symbols) <- get
    push (Integer 7)
    pop
    push (String "test")
    register ("test", 15)
    remove "test"

  main :: IO()
  main = do
    let res = runState (stackTests) emptyMemory
    print res