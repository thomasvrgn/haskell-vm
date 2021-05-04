{-# LANGUAGE BlockArguments #-}
module Types.Stack where
  import Types.Value
  import Control.Monad.State
  import Data.List (find)

  type Stack = [Value]

  type Address = Int  
  type Symbol = (String, Address)
  type Symbols = [Symbol]

  type Memory = (Stack, Symbols)

  intLength :: Show a => a -> Int
  intLength = length . show

  -- Memory methods
  createTabulation :: Int -> String
  createTabulation len = replicate len ' '

  printMem :: Memory -> IO()
  printMem (stack, symbols) = do
    if length stack > 0 then putStrLn "Stack:" else return ()
    mapM_ (\(index, name) -> putStrLn $ "  " ++ show index ++ ". " ++ case name of
      (String str) -> "String " ++ show str
      (Integer int) -> "Integer " ++ show int) (zip [0..] stack)
    
    if length symbols > 0 then putStrLn "\nSymbols:" else return ()
    mapM_ (\(index, (name, address)) -> do
      putStrLn $ "  " ++ show index ++ ". Name: " ++ name
      putStrLn $ "  " ++ (createTabulation . intLength $ index) ++ "  Address: " ++ show address
      ) (zip [0..] symbols)

  -- Stack methods
  push :: Value -> State Memory ()
  push value = state \(stack, symbols) -> ((), (stack ++ [value], symbols))

  peek :: State Memory Value
  peek = state \memory@(stack, _) -> (last stack, memory)

  pop :: State Memory Value
  pop = state \(stack, symbols) -> (last stack, (init stack, symbols))

  clean :: State Memory ()
  clean = state \s -> ((), s)

  stackLength :: State Memory Int
  stackLength = state \(stack, symbols) -> ((length stack) - 1, (stack, symbols))

  -- Symbols methods
  register :: Symbol -> State Memory ()
  register sym@(name, _) = state \(stack, symbols) -> 
    ((), (stack, (filter ((/=name) . fst) symbols) ++ [sym]))

  remove :: String -> State Memory Symbol
  remove name = state \(stack, symbols) -> 
    (getVariable symbols, (stack, removeVariable symbols))
    where getVariable = (!!0) . filter ((==name) . fst)
          removeVariable = filter ((/=name) . fst)

  getVariable :: String -> State Memory Symbol
  getVariable name = state \memory@(_, symbols) -> (let var = (find ((==name) . fst) symbols) in case var of
    Nothing -> ("", -1)
    (Just x) -> x, memory)

  -- Initializer functions
  emptyStack :: Stack
  emptyStack = []

  emptySymbols :: Symbols
  emptySymbols = []

  emptyMemory :: Memory
  emptyMemory = (emptyStack, emptySymbols)
