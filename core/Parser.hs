module Core.Parser where
  import Types.Bytecode
  import Types.Stack
  import Types.Value

  import Core.Lexer

  import Data.Char
  import Data.Typeable
  import Data.Maybe
  import Data.List

  endWiths :: Char -> String -> Bool
  endWiths x = (==x) . last

  joinSpace :: [String] -> String
  joinSpace = concat . intersperse " "

  isAllUpper :: String -> Bool
  isAllUpper = all isUpper

  removeEmptyLists :: [[a]] -> [[a]]
  removeEmptyLists = filter ((/=0) . length)

  type Segments = [(String, [String])]
  type Expression = [String]

  regroup :: [String] -> [Expression]
  regroup content = parse' content ([], [[]])
    where parse' :: [String] -> ([String], [Expression]) -> [[String]]
          parse' (x:xs) (state, ast) =
            if isAllUpper x
              then parse' xs ([x], ast ++ [state])
              else parse' xs (state ++ [x], ast)
          parse' [] (state, ast) = removeEmptyLists $ ast ++ [state]

  process :: [String] -> Segments
  process x = parse' x ([], [])
    where parse' :: [String] -> ([String], Segments) -> Segments
          parse' [] (content, segments) = let (seg:instr) = content in segments ++ [(seg, instr)]
          parse' (x:xs) (el, seg) = if endWiths ':' x
            then if length el > 0 
              then let (seg':content) = el in parse' xs ([init x], seg ++ [(seg', content)])
              else parse' xs ([init x], seg)
            else parse' xs (el ++ [x], seg)

  readMaybe :: (Read a) => String -> Maybe a
  readMaybe s = case reads s of
                  [(x, "")] -> Just x
                  _ -> Nothing

  valueify :: String -> Value
  valueify elem = case (readMaybe elem) of
    (Just a) -> Integer a
    Nothing -> String ((drop 1) . init $ elem)

  transform :: [Expression] -> Bytecode
  transform [] = []
  transform (x:xs) = 
    (let (expr:args) = x in case uppercase expr of
      "LOAD" -> let (name:_) = args in [Load name]
      "LOAD_SECTION" -> let (section:_) = args in [LoadSection section]

      "PUSH" -> let (elem:_) = args in [Push (valueify elem)]
      "POP" -> [Pop]

      "STORE" -> let (name:_) = args in [Store name]
      "DROP" -> let (name:_) = args in [Drop name]
      "CALL" -> let (argsLength:_) = args in [Call (read argsLength :: Int)]

      "ADD" -> [Add]
      "MUL" -> [Mul]
      "SUB" -> [Sub]
      "DIV" -> [Div]

      _ -> error ("Can't recognize instruction " ++ expr ++ "!")
      ) ++ transform xs

  parse :: String -> [(String, Bytecode)]
  parse = map (fmap (transform . regroup)) . process . tokenize