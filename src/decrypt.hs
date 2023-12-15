module Decrypt
    (   isPDF,
        isEncrypted
    ) where

import           Data.Char        (toLower)
import           Data.List        (isSuffixOf)
import           System.Directory
import           System.IO
import           System.Process

-- >>> isPDF "test.pdf"
-- True

isPDF :: String -> IO Bool
isPDF "" = return False
isPDF path = do
    r <- (&&) <$> doesPathExist path <*> doesFileExist path
    return $ r && ".pdf" `isSuffixOf` map toLower path

isEncrypted "" = return (False, Nothing)
isEncrypted path = do
    current <- isPDF path
    if current
        then do
            (_, Just hout, _, _) <- createProcess (proc "exiftool" ["-s", "-T", "-Encryption", path]) { std_out = CreatePipe }
            out <- hGetContents hout
            if length out > 3 -- この数は、'-'と改行文字'\n'と合わせたもの
                then return (True, Just hout)
                else return (False, Nothing)
        else
            return (False, Nothing)