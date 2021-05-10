module Core.Parser where
  import Types.Bytecode
  import Types.Stack
  import Types.Value

  import Data.Char
  import Data.Typeable
  import Data.Maybe
  import Data.Char

  endWiths :: Char -> String -> Bool
  endWiths x = (==x) . last

  type Segments = [(String, [String])]

  parse :: String -> Segments
  parse x = parse' split ([], [])
    where split  = words x
          parse' :: [String] -> ([String], Segments) -> Segments
          parse' [] (content, segments) = let (seg:instr) = content in segments ++ [(seg, instr)]
          parse' (x:xs) (el, seg) = if endWiths ':' x
            then if length el > 0 
              then let (seg':content) = el in parse' xs ([x], seg ++ [(seg', content)])
              else parse' xs ([x], seg)
            else parse' xs (el ++ [x], seg)