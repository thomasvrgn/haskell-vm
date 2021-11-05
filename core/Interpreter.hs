{-# LANGUAGE NamedFieldPuns, ConstraintKinds, FlexibleContexts #-}
module Core.Interpreter where
  import Types.Value
  import Core.Parser (Bytecode(..))
  import Control.Monad.State
  import qualified Data.List as L

  -- Some basic VM types
  data Stack a = Stack {
    sp :: Int,
    content :: [a],
    size :: Int
  } deriving Show

  data Code = Code {
    ip :: Int,
    program :: [Bytecode]
  } deriving Show

  data Program a = Program {
    code :: Code,
    stack :: Stack a
  } deriving Show

  type VM m a = (MonadIO m, MonadState (Program a) m, MonadFail m)

  -- Some util functions
  initStack :: Int -> Stack a
  initStack i = Stack {
    sp = -1,
    content = [],
    size = i
  }

  getRule :: String -> [Bytecode] -> Maybe Bytecode
  getRule s = L.find ((==s) . rule)

  (<%>) = getRule

  initProgram :: [Bytecode] -> Program a
  initProgram xs =
    Program {
      stack = initStack s,
      code = Code {
        ip = i,
        program = code
      }
    }
    where s = case "size" <%> xs of
                Just Rule { rule = "size", value = s' } -> fromInteger s'
                Just _ -> error "Cannot happen!"
                Nothing -> 255
          i = case "index" <%> xs of
                Just Rule { rule = "index", value = i' } -> fromInteger i'
                Just _ -> error "Cannot happen!"
                Nothing -> 0

          isInstruction (Instruction _ _) = True
          isInstruction _ = False

          code = filter isInstruction xs

  -- VM useful functions
  push :: VM m a => a -> m ()
  push v = do
    Program { stack, code } <- get
    unless (sp stack < size stack - 1) $
      error "Stack overflow"
    let stack' = Stack {
        sp = sp stack + 1,
        content = content stack ++ [v],
        size = size stack
      } in put $ Program { stack = stack', code }
    return ()

  pop :: VM m a => m a
  pop = do
    Program { stack, code } <- get
    unless (sp stack >= 0) $
      error "Stack underflow"
    let stack' = Stack {
        sp = sp stack - 1,
        content = tail $ content stack,
        size = size stack
      } in put $ Program { stack = stack', code }
    return . head $ content stack

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
    Program { code, stack } <- get
    unless (i >= 0 && i < length (program code)) $
      error "Invalid IP"
    put $ Program { code = code { ip = i }, stack }
    return ()

  getProgramLength :: VM m a => m Int
  getProgramLength = do
    Program { code } <- get
    return $ length $ program code

  incIP :: VM m a => m ()
  incIP = getIP >>= setIP . (+1)

  getInstruction :: VM m a => m Bytecode
  getInstruction = do
    Program { code, stack } <- get
    let i = ip code
    return $ program code !! i

  run :: VM m Integer => m ()
  run = do
    Instruction { instruction = instr, arguments = arguments } <- getInstruction
    
    case instr of
      "PUSH" -> push $ head arguments
      "EXTERN" -> case head arguments of
                    1 -> pop >>= liftIO . print
                    _ -> error "Extern not found!"
      "POP" -> void pop
      "ADD" -> ((+) <$> pop <*> pop) >>= push
      "JUMP" -> setIP . (\x -> x - 1) . fromInteger . head $ arguments
      _ -> error "Instruction not found!"

    -- Incrementing IP if not overflowing bytecode
    l <- getProgramLength
    i <- getIP
    when (i < l - 1) $ do
      incIP
      run




  