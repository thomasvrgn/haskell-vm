{-# LANGUAGE BlockArguments #-}
module Main where
  data Value
    = String String
    | Integer Integer
    deriving (Show)

  type Stack = [Value]

  main :: IO ()
  main = print (String "test")