import Relude

import Text.Gigaparsec

import Ast qualified
import Lam

main :: IO ()
main = do
    input <- getLine
    expression <- eval (toString input)
    putTextLn . show $ Ast.eval expression
    main

eval :: String -> IO Ast.Expr
eval input =
    case parse Lam.expr input of
        Success e -> pure e
        Failure -> do
            -- putStrLn (errorBundlePretty err)
            putStrLn "Try Identity: x = x"
            eval "x = x"
