module Core.Parser where
  import Types.Bytecode
  import Types.Stack
  import Types.Value
  import Data.Char

  isAllUpper :: String -> Bool
  isAllUpper = all isUpper

  removeEmptyLists :: [[a]] -> [[a]]
  removeEmptyLists = filter ((/=0) . length)

  type Expression = [String]

  parse :: String -> [Expression]
  parse content = parse' (words content) ([], [[]])
    where parse' :: [String] -> ([String], [Expression]) -> [[String]]
          parse' (x:xs) (state, ast) =
            if (isAllUpper x) 
              then parse' xs ([x], ast ++ [state])
              else parse' xs (state ++ [x], ast)
          parse' [] (state, ast) = removeEmptyLists $ ast ++ [state]