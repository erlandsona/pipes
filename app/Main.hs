import Relude

import Data.String.Interpolate (i)

import Lib

main :: IO ()
main = do
    result <- go "test/MyProgram.|"
    print result
