{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module ArgumentsParser (parseArgs) where

parseArgs :: String -> IO ()
parseArgs "" = putStrLn "Please input ..."
parseArgs _ = putStrLn "Invalid arguments " ++ "'" ++ _ ++ "'"