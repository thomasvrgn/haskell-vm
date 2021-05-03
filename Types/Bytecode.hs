module Types.Bytecode where
  import Types.Value

  data Instruction
    -- Variable and value related
    = Push Value
    | Pop
    | Load String
    | Store String
    | Call Int

    -- Mathematics related
    | Add
    | Mul
    | Div
    | Sub

  type Bytecode = [Instruction]