module Core.Parser where
  import Types.Stack
  import Types.Value

  import Core.Lexer
  import Data.Char
  import Data.List

  startsWith :: Eq a => a -> [a] -> Bool
  startsWith d (x:_) = d == x

  data Bytecode
    = Rule {
        rule  :: String,
        value :: Integer
      }
    | Instruction {
        instruction :: String,
        arguments   :: [Integer]
      }
    deriving Show

  split :: Char -> String -> [String]
  split _ [] = []
  split d x = c : rest
    where (c, r) = span (/=d) x
          r'     = drop 1 r
          rest   = split d r'

  format :: String -> [[String]]
  format = filter (not . null) . map (filter (/="") . split ' ') . lines
  -- lines just split string over new lines
  -- this function is just splitting raw bytecode string into organized array of array of elements

  line :: [String] -> Bytecode
  line (instr:args) =
    if startsWith '@' instr
      then parseRule instr args
      else parseInstruction instr args

  parseRule :: String -> [String] -> Bytecode
  parseRule instr (arg:_) = Rule {
    rule = drop 1 instr,
    value = read arg
  }

  parseInstruction :: String -> [String] -> Bytecode
  parseInstruction instr args = Instruction {
    instruction = instr,
    arguments = read <$> args
  }

  parse = map line . format