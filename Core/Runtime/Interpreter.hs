{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.Runtime.Interpreter where
  import Types.Value
  import Control.Monad.State
  import qualified Data.List as L
  import Core.Error.Error (throw, debugStack)
  import Core.Error.Types (Error(..))
  import Core.Runtime.VM
  import System.Console.ANSI

  -- Some util functions
  initStack :: Int -> Stack a
  initStack i = Stack {
    sp = -1,
    content = [],
    size = i
  }

  getRule :: String -> [Bytecode a] -> Maybe (Bytecode a)
  getRule s = L.find ((==s) . rule)

  (<%>) = getRule

  initProgram :: [Bytecode a] -> Program a
  initProgram xs =
    Program {
      stack = initStack s,
      code = Code {
        ip = i,
        program = code
      },
      symbols = Symbols {
        symbolsContent = []
      }
    }
    where s = case "size" <%> xs of
                Just Rule { rule = "size", value = s' } -> fromInteger s'
                Just _ -> error "Invalid program"
                Nothing -> 255
          i = case "index" <%> xs of
                Just Rule { rule = "index", value = i' } -> fromInteger i'
                Just _ -> error "Invalid program"
                Nothing -> 0

          isInstruction (Instruction _ _) = True
          isInstruction _ = False

          code = filter isInstruction xs

  -- VM useful functions
  push :: VM m a => a -> m ()
  push v = do
    Program { stack, code, symbols } <- get
    unless (sp stack < size stack - 1) $ void $ throw StackOverflow
    let stack' = Stack {
        sp = sp stack + 1,
        content = content stack ++ [v],
        size = size stack
      } in put $ Program { stack = stack', code, symbols }
    return ()

  pop :: VM m a => m a
  pop = do
    Program { stack, code, symbols } <- get
    unless (sp stack >= 0) $ void $ throw StackUnderflow
    let stack' = Stack {
        sp = sp stack - 1,
        content = init $ content stack,
        size = size stack
      } in put $ Program { stack = stack', code, symbols }
    return . last $ content stack

  register :: VM m a => String -> a -> m ()
  register s v = do
    Program { symbols, stack, code } <- get
    put $ Program { symbols = Symbols { symbolsContent = (s, v) : symbolsContent symbols }, stack, code }

  lookup :: VM m a => String -> m a
  lookup s = do
    Program { symbols } <- get
    case Prelude.lookup s (symbolsContent symbols) of
      Just v -> return v
      Nothing -> throw $ NotFound "Symbol"

  unregister :: VM m a => String -> m ()
  unregister s = do
    Program { symbols, stack, code } <- get
    put $ Program { symbols = Symbols { symbolsContent = filter ((/=s) . fst) (symbolsContent symbols) }, stack, code }

  stackSize :: VM m a => m Int
  stackSize = do
    Program { stack } <- get
    return $ size stack

  getIP :: VM m a => m Int
  getIP = do
    Program { code } <- get
    return $ ip code

  setIP :: VM m a => Int -> m ()
  setIP i = do
    Program { code, stack, symbols } <- get
    unless (i >= 0 && i < length (program code)) $
      void $ throw InvalidInstructionPointer
    put $ Program { code = code { ip = i }, stack, symbols }
    return ()

  getCode :: VM m a => m [Bytecode a]
  getCode = do
    Program { code } <- get
    return $ program code

  getProgramLength :: VM m a => m Int
  getProgramLength = do
    Program { code } <- get
    return $ length $ program code

  incIP :: VM m a => m ()
  incIP = getIP >>= setIP . (+1)

  slice xs i k = map (xs!!) [i-1 .. k-1]

  getInstruction :: VM m a => m (Bytecode a)
  getInstruction = do
    Program { code, stack } <- get
    let i = ip code
    return $ program code !! i

  run :: VM m Value => m ()
  run = do
    Instruction { instruction = instr, arguments = arguments } <- getInstruction

    case instr of
      "PUSH" -> push . head $ arguments

      "STORE" -> do
        v <- pop
        let (name:_) = arguments in case name of
          String n -> register n v
          _ -> void . throw $ Error "Variable name must be a String"

      "LOAD" -> do
        let (name:_) = arguments in case name of
          String n -> Core.Runtime.Interpreter.lookup n >>= push
          _ -> void . throw $ Error "Variable name must be a String"

      "EXTERN" -> do
        let (name:_) = arguments in case name of
          Integer 1 -> pop >>= liftIO . print
          Integer 0 -> do
            Stack { sp, content, size } <- gets stack
            i <- getIP
            liftIO $ do
              setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
              putStr "Debug: "

              setSGR [Reset, SetColor Foreground Vivid Green]
              putStrLn $ "Stack at IP " ++ show i

              debugStack (content, sp, size)
          _ -> void . throw $ NotFound "Implementation"

      "MAKE_FUNCTION" -> do
        let (Integer qtInstrs:_) = arguments
        bytecode <- getCode
        i <- getIP
        push $ Integer . toInteger $ i + 1
        setIP $ i + fromInteger qtInstrs

      "CALL" -> do
        let (Integer qtArgs:_) = arguments
        args <- replicateM (fromInteger qtArgs) pop
        Integer ip <- pop

        getIP >>= push . Integer . toInteger
        setIP $ fromInteger (ip - 1)
        mapM_ push args

      "RETURN" -> do
        res <- pop
        Integer ip <- pop
        setIP $ fromInteger ip
        push res

      "RETURN_UNIT" -> do
        Integer ip <- pop
        setIP $ fromInteger ip

      "JUMP" -> do
        let (Integer ip:_) = arguments
        setIP $ fromInteger (ip - 1)
      
      "JUMP_RELATIVE" -> do
        let (Integer ip:_) = arguments
        getIP >>= setIP . ((+) . fromInteger $ ip - 1)

      "ADD" -> push =<< ((+) <$> pop) <*> pop

      _ -> void . throw $ NotFound "Instruction"

    -- Incrementing IP if not overflowing bytecode
    l <- getProgramLength
    i <- getIP
    when (i < l - 1) $ do
      incIP
      run




  