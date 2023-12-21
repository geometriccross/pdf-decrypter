module Decrypt
    (   isPDF,
        checkFileInfo,
        decrypt
    ) where

import           Data.Char        (toLower)
import           Data.List        (isSuffixOf)
import           System.Directory
import           System.IO
import           System.Process
import Control.Applicative (Applicative(liftA2))


isPDF :: FilePath -> IO Bool
isPDF "" = return False
isPDF path = do
    r <- (&&) <$> doesPathExist path <*> doesFileExist path
    return $ r && ".pdf" `isSuffixOf` map toLower path

checkFileInfo :: FilePath -> String -> IO Bool
checkFileInfo "" _ = return False
checkFileInfo path arg = do
    current <- isPDF path
    if current
        then do
            (_, Just hout, _, _) <- createProcess (proc "exiftool" ["-s", "-T", arg, path]) { std_out = CreatePipe }
            out <- hGetContents hout
            return $ length out > 3 -- この数は、'-'と改行文字'\n'と合わせたもの
        else return False

decrypt :: FilePath -> FilePath -> [String] -> IO ()
decrypt "" _ _ = return ()
decrypt _ "" _ = return ()
decrypt _ _ [] = return ()
decrypt input_path out_path (p:px) = do
    let process = proc "qpdf" ["--decrypt", input_path, out_path, "--password=" ++ p]
    (_, Just hout, _, _) <- createProcess process { std_out = CreatePipe }
    out <- hGetContents hout
    if length out > 2
        then return ()
        else decrypt input_path out_path px
