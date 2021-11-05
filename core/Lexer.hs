module Core.Lexer where
  import Data.Char

  type Span = (String, String)

  replaceNewLine :: String -> Char -> String
  replaceNewLine xs d = [if x `elem` " \n" then d else x | x <- xs]

  uppercase :: String -> String
  uppercase = map toUpper

  isChar :: Char -> Bool
  isChar = flip elem (['a'..'z'] ++ ['A'..'Z'])

  string :: String -> Span
  string = span (/= '"')

  word :: String -> Span
  word= span (/=' ')

  number :: String -> Span
  number = span (\x -> (isDigit x || x == '.') && x /= ' ')

  lexer :: String -> [String]
  lexer [] = []
  lexer str@(x:xs)
    | x == '"' = let (s1,s2) = string xs in ('"' : s1 ++ "\"") : lexer (drop 1 s2)
    | isDigit x = let (n1,n2) = number xs in (x : n1) : lexer n2
    | isChar x = let (w1,w2) = word str in w1 : lexer w2
    | x == ' ' = lexer xs
    | otherwise = [x] : lexer xs

  tokenize :: String -> [String]
  tokenize str = lexer $ replaceNewLine str ' '