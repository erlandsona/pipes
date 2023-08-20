import Relude

import Data.String.Interpolate (i)
import Parsley

import Ast
import Lib

go :: String -> Maybe (Exp String)
go = $$(parse varParser)

main :: IO ()
main = do
  let a :: Word = 1
  let b = 2
  let _ = stuff
  putStrLn [i|Sup #{a + b}|]
