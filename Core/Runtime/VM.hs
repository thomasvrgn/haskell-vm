{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.Runtime.VM where
  import Control.Monad.State
  import Types.Value

  -- Some basic VM types
  data Stack a = Stack {
    sp :: Int,
    content :: [a],
    size :: Int
  } deriving Show

  data Code a = Code {
    ip :: Int,
    program :: [Bytecode a]
  } deriving Show

  newtype Symbols a = Symbols {
    symbolsContent :: [(String, a)]
  } deriving Show

  data Program a = Program {
    code :: Code a,
    stack :: Stack a,
    symbols :: Symbols a
  } deriving Show

  type VM m a = (Show a, Num a, MonadIO m, MonadState (Program a) m, MonadFail m)