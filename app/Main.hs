module Main where

import Relude

import Text.Megaparsec

import Ast qualified
import Lam qualified

main :: IO ()
main = do
    -- Read
    input <- getLine
    -- Evaluate
    let
        output = eval input
    -- Print
    putTextLn output
    -- Loop
    main

eval :: Text -> Text
eval input =
    case parse Lam.expr "" input of
        Right e ->
            show @Text $ Ast.nf e
        Left err -> do
            toText (errorBundlePretty err)
