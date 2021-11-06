{-# LANGUAGE BlockArguments, TypeApplications #-}
module Main where
  import Core.Runtime.Interpreter
  import Core.Analysis.Parser
  import Core.Analysis.Lexer

  import Types.Value

  import Control.Monad.State

  main :: IO()
  main = do
    content <- readFile "tests/Sample.hsc"
    let bytecode = parse content
    let p = initProgram @Value bytecode
    (res, p1) <- runStateT run p
    print res