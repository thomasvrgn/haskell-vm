{-# LANGUAGE BlockArguments #-}
module Types.Stack where
  import Types.Value
  import Control.Monad.State

  type Stack = [Value]

  type Address = Int  
  type Symbol = (String, Address)
  type Symbols = [Symbol]

  type Memory = (Stack, Symbols)
  
  -- Stack methods
  push :: Value -> State Memory ()
  push value = state \(stack, symbols) -> ((), (stack ++ [value], symbols))

  peek :: State Memory Value
  peek = state \memory@(stack, _) -> (last stack, memory)

  pop :: State Memory Value
  pop = state \(stack, symbols) -> (last stack, (init stack, symbols))

  clean :: State Memory ()
  clean = state \s -> ((), s)

  -- Symbols methods
  register :: Symbol -> State Memory ()
  register sym@(name, _) = state \(stack, symbols) -> 
    ((), (stack, (filter ((/=name) . fst) symbols) ++ [sym]))

  remove :: String -> State Memory Symbol
  remove name = state \(stack, symbols) -> 
    (getVariable symbols, (stack, removeVariable symbols))
    where getVariable = (!!0) . filter ((==name) . fst)
          removeVariable = filter ((/=name) . fst)
  
  stackLength :: State Memory Int
  stackLength = state \(stack, symbols) -> ((length stack) - 1, (stack, symbols))

  -- Initializer functions
  emptyStack :: Stack
  emptyStack = []

  emptySymbols :: Symbols
  emptySymbols = []

  emptyMemory :: Memory
  emptyMemory = (emptyStack, emptySymbols)
