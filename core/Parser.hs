module Core.Parser where
  import Types.Bytecode
  import Types.Stack
  import Types.Value
  import Data.Char
  import Data.Typeable
  import Data.Maybe
  import Data.Char

  maybeRead2 :: Read a => String -> Maybe a
  maybeRead2 = fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads

  isAllUpper :: String -> Bool
  isAllUpper = all isUpper

  removeEmptyLists :: [[a]] -> [[a]]
  removeEmptyLists = filter ((/=0) . length)

  type Expression = [String]

  regroup :: String -> [Expression]
  regroup content = parse' (words content) ([], [[]])
    where parse' :: [String] -> ([String], [Expression]) -> [[String]]
          parse' (x:xs) (state, ast) =
            if (isAllUpper x) 
              then parse' xs ([x], ast ++ [state])
              else parse' xs (state ++ [x], ast)
          parse' [] (state, ast) = removeEmptyLists $ ast ++ [state]
  

  valueify :: String -> Value
  valueify elem = case (maybeRead2 elem) of
    (Just a) -> Integer a
    Nothing -> String ((drop 1) . init $ elem)

  process :: [Expression] -> Bytecode
  process [] = []
  process (x:xs) = 
    (let (expr:args) = x in case expr of
      "PUSH" -> let (elem:_) = args in [Push (valueify elem)]
      "STORE" -> let (name:_) = args in [Store name]
      "LOAD" -> let (name:_) = args in [Load name]
      "DROP" -> let (name:_) = args in [Drop name]
      ) ++ process xs

  parse :: String -> Bytecode
  parse = process . regroup