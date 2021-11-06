module Core.Analysis.Parser where
  import Types.Value

  import Core.Analysis.Lexer
  import Data.Char
  import Data.List

  startsWith :: Eq a => a -> [a] -> Bool
  startsWith _ [] = False
  startsWith d (x:_) = d == x

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

  parseArgs :: [String] -> [Value]
  parseArgs = map parseArg

  parseArg :: String -> Value
  parseArg s = if startsWith '"' s
    then String . tail . init $ s
    else Integer . read $ s

  line :: [String] -> Bytecode Value
  line [] = Comment
  line (instr : args)
    | startsWith '@' instr = parseRule instr args
    | not $ startsWith '#' instr = parseInstruction instr args
    | otherwise = Comment

  parseRule :: String -> [String] -> Bytecode a
  parseRule _ [] = error "Rule cannot be without argument"
  parseRule instr (arg:_) = Rule {
    rule = drop 1 instr,
    value = read arg
  }

  parseInstruction :: String -> [String] -> Bytecode Value
  parseInstruction instr args = Instruction {
    instruction = instr,
    arguments = parseArgs args
  }

  parse = map line . format