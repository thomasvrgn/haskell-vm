{-# LANGUAGE BlockArguments #-}
module Main where
  import Control.Monad.State

  data Value
    = String String
    | Integer Integer
    deriving (Show)

  type Stack = [Value]

  type Address = Int
  type Pointer = (String, Address)
  type Symbols = [Pointer]

  data Instruction
    = PUSH Value
    | POP
    | STORE String

  main :: IO ()
  main = print (String "test")