module Types.Stack where
  import Types.Value

  type Stack = [Value]

  type Address = Int  
  type Symbol = (String, Address)
  type Symbols = [Symbol]

  type Memory = (Stack, Symbols)
  
  -- Stack methods
  push :: Value -> Stack -> Stack
  push value stack = stack ++ [value]

  getLast :: Stack -> Value
  getLast = last

  removeLast :: Stack -> Stack
  removeLast = init

  pop :: Stack -> (Value, Stack)
  pop = (,) <$> getLast <*> removeLast

  -- Symbols methods
  register :: Symbol -> Symbols -> Symbols
  register (name, address) symbols = symbols ++ [(name, address)]

  remove :: String -> Symbols -> Symbols
  remove name = filter ((/=name) . fst)