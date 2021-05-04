module Core.Interpreter where
  import Types.Value
  import Types.Stack
  import Types.Bytecode
  import Control.Monad.State
  
  runBytecode :: Bytecode -> State Memory ()
  runBytecode [] = return ()
  runBytecode (x:xs) = evalInstruction x
    where 
      evalInstruction (Push value) = do
        push value
        runBytecode xs

      evalInstruction (Store name) = do
        index <- stackLength
        register (name, index)
        runBytecode xs
      
      evalInstruction (Load name) = do
        (_, index) <- getVariable name
        (stack, _) <- get
        push (stack !! index)
        runBytecode xs

      evalInstruction (Drop name) = do
        remove name
        runBytecode xs