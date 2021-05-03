module Types.Stack where
  import Types.Value

  type Stack = [Value]

  type Address = Int  
  type Symbol = (String, Address)
  type Symbols = [Symbol]

  type Memory = (Stack, Symbols)
  