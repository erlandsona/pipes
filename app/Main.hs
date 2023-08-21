import Relude

import Data.String.Interpolate (i)
import Parsley

import Ast
import Lib

main :: IO ()
main = do
  stuff <- go "test/MyProgram.|"
  putStrLn $ show stuff
