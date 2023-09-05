module Main where

import Relude

import Text.Megaparsec

import Lam

main :: IO ()
main = do
    input <- getLine
    expression <- eval input
    putTextLn . show @Text $ nf expression
    main

eval :: Text -> IO Expr
eval input =
    case parse Lam.expr "" input of
        Right e -> pure e
        Left err -> do
            putStrLn (errorBundlePretty err)
            putStrLn "Try Identity: \\x. x"
            eval "\\x. x"
