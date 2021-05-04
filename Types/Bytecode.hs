module Types.Bytecode where
  import Types.Value

  type Name = String
  type ArgsLength = Int
  data Instruction
    -- Variable and value related
    = Push Value
    | Pop
    | Load Name
    | Store Name
    | Call ArgsLength
    | Drop Name

    -- Mathematics related
    | Add
    | Mul
    | Div
    | Sub

  type Bytecode = [Instruction]