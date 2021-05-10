module Core.Parser where
  import Types.Bytecode
  import Types.Stack
  import Types.Value

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

  parse :: String -> Segments
  parse x = parse' split ([], [])
    where split  = words x
          parse' :: [String] -> ([String], Segments) -> Segments
          parse' [] (content, segments) = let (seg:instr) = content in segments ++ [(seg, instr)]
          parse' (x:xs) (el, seg) = if endWiths ':' x
            then if length el > 0 
              then let (seg':content) = el in parse' xs ([init x], seg ++ [(seg', content)])
              else parse' xs ([init x], seg)
            else parse' xs (el ++ [x], seg)
