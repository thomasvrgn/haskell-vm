module Types.Value where
  import Data.Char
  import Data.List (intersperse)
  import System.Console.ANSI
  data Bytecode a
    = Rule {
        rule  :: String,
        value :: Integer
      }
    | Instruction {
        instruction :: String,
        arguments   :: [a]
      }
    | Comment
    deriving Show

  toUpperCase :: String -> String
  toUpperCase = map toUpper
  
  data Value
    = String String
    | Integer Integer
  
  instance Show Value where
    show (String s) = 
      setSGRCode [SetColor Foreground Dull Green] ++ show s ++ setSGRCode [Reset]
    show (Integer i) =
      setSGRCode [SetColor Foreground Vivid Yellow] ++ show i ++ setSGRCode [Reset]

  instance Num Value where
    Integer a + Integer b = Integer $ a + b
    String a + String b = String $ a ++ b
    _ + _ = error "Can't add non-integers"

    Integer a * Integer b = Integer $ a * b
    String a * String b = String $ a ++ b
    _ * _ = error "Can't multiply non-integers"

    abs (Integer a) = Integer $ abs a
    abs _ = error "Can't take the absolute value of non-integers"

    signum (Integer a) = Integer $ signum a
    signum _ = error "Can't take the sign of non-integers"

    fromInteger a = Integer a

    negate (Integer a) = Integer $ negate a
    negate _ = error "Can't negate non-integers"