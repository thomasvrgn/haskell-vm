{-# LANGUAGE BlockArguments #-}
module Main where
  import Control.Monad.State

  data Value
    = String String
    | Integer Integer
    deriving (Show)

  type Frame = [Value]

  type Address = Int
  type Pointer = (String, Address)
  type Symbols = [Pointer]

  type Stack = [Frame]
  type Memory = (Symbols, Stack)

  data Instruction
    = PUSH Value
    | POP
    | STORE String

  type Page = [Instruction]

  main :: IO ()
  main = print (String "test")