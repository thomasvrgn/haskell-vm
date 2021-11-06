module Core.Error.Types where
  data Error
    = StackOverflow
    | StackUnderflow
    | InvalidInstruction
    | NotFound String
    | Error String
    | InvalidInstructionPointer

  instance Show Error where
    show StackOverflow = "Stack overflow"
    show StackUnderflow = "Stack underflow"
    show InvalidInstruction = "Invalid instruction"
    show (NotFound t) = t ++ " not found"
    show (Error msg) = msg
    show InvalidInstructionPointer = "Invalid instruction pointer"