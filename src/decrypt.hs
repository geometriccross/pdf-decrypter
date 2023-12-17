module Decrypt
    (   isPDF,
        isEncrypted
    ) where

import           Data.Char        (toLower)
import           Data.List        (isSuffixOf)
import           System.Directory
import           System.IO
import           System.Process
import System.FilePath (takeBaseName)

-- >>> isPDF "test.pdf"
-- True

isPDF :: String -> IO Bool
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

suffixChange :: FilePath -> String -> FilePath
suffixChange path suffix = takeBaseName . (++ "." ++ suffix) $ path

decrypt :: FilePath -> [String] -> IO (Maybe FilePath)
decrypt "" _ = return Nothing
decrypt _ [] = return Nothing
decrypt path (p:px) = do
    let process = proc "qpdf" ["--decrypt", path, suffixChange path "temp", "--password=" ++ p]
    (_, Just hout, _, _) <- createProcess process { std_out = CreatePipe }
    out <- hGetContents hout
    if length out > 2
        then do
            removeFile path
            renameFile (suffixChange path "temp") path
            return $ Just path
        else decrypt path px