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
    deriving Show

  type Bytecode = [Instruction]

  -- Bytecode methods
  getValue :: Value -> String
  getValue (String t) = t
  getValue (Integer t) = show t

  printBytecode :: Bytecode -> IO()
  printBytecode [] = return ()
  printBytecode (x:xs) = do
    case x of 
      Push a -> putStr $ "PUSH " ++ getValue a
      Store a -> putStr $ "STORE " ++ a
      Load a -> putStr $ "LOAD " ++ a
      Drop a -> putStr $ "DROP " ++ a
    putStrLn []
    printBytecode xs