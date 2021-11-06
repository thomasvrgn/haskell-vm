{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MonoLocalBinds #-}
module Core.Error.Error where
  import Core.Error.Types
  import Core.Runtime.VM
  import Types.Value
  import Control.Monad.State
  import System.Console.ANSI
  import System.Exit
  import GHC.Stack
  
  throw :: VM m a => Error -> m a
  throw x = do
    Program { 
      code = Code { ip, program },
      stack = Stack { content, sp, size } } <- get
    let Instruction { instruction, arguments } = program !! ip

    liftIO $ do
      setSGR [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
      putStr "Error: "

      setSGR [Reset, SetColor Foreground Vivid Red]
      print x

      setSGR [Reset]
      let instr = setSGRCode [SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity] ++ instruction ++ setSGRCode [Reset]

      setSGR [SetColor Foreground Vivid Black]
      putStr $ " At IP " ++ show ip ++ " → "
      putStr instr
      putStrLn $ " " ++ unwords (map show arguments)

      debugStack (content, sp, size)

      exitFailure 
      fail "test"

  debugStack :: Show a => ([a], Int, Int) -> IO ()
  debugStack (content, sp, size) = do
      setSGR [Reset, SetColor Foreground Vivid Black]
      putStrLn " Stack:"
      putStrLn $ "   → Content: " ++ setSGRCode [Reset] ++ show content
      setSGR [SetColor Foreground Vivid Black]
      putStrLn $ "   → Pointer: " ++ setSGRCode [Reset] ++ show sp
      setSGR [SetColor Foreground Vivid Black]
      putStrLn $ "   → Max size: " ++ setSGRCode [Reset] ++ show size