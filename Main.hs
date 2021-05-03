{-# LANGUAGE BlockArguments #-}
module Main where
  import Types.Value
  import Types.Stack
  import Types.Bytecode

  main :: IO()
  main = do
    let (stack, symbols) = emptyMemory
    print stack2
    let stack2 = push (String "test") stack
    print stack2