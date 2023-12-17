module Decrypt
    (   isPDF,
        isEncrypted,
        decrypt
    ) where

import           Data.Char        (toLower)
import           Data.List        (isSuffixOf)
import           System.Directory
import           System.IO
import           System.Process


isPDF :: FilePath -> IO Bool
isPDF "" = return False
isPDF path = do
    r <- (&&) <$> doesPathExist path <*> doesFileExist path
    return $ r && ".pdf" `isSuffixOf` map toLower path

isEncrypted :: FilePath -> IO Bool
isEncrypted "" = return False
isEncrypted path = do
    current <- isPDF path
    if current
        then do
            (_, Just hout, _, _) <- createProcess (proc "exiftool" ["-s", "-T", "-Encryption", path]) { std_out = CreatePipe }
            out <- hGetContents hout
            if length out > 3 -- この数は、'-'と改行文字'\n'と合わせたもの
                then return True
                else return False
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
