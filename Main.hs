{-# LANGUAGE BlockArguments #-}
module Main where
  data Value
    = String String
    | Integer Integer
    deriving (Show)

  type Stack = [Value]

  data Instruction
    = PUSH Value
    | POP
    | STORE String

  main :: IO ()
  main = print (String "test")