module Decrypt 
    (   isPDF, 
        isEncrypted
    ) where

import           System.Directory
import           System.IO
import           System.Process
import Data.Char (toLower)
import Data.List (isSuffixOf)

-- >>> isPDF "test.pdf"
-- True

isPDF :: String -> Bool
isPDF s = ".pdf" `isSuffixOf` map toLower s

-- >>> isEncrypted "invailde/path/to/file.pdf"
-- (False, Nothing)
--

isEncrypted :: String -> IO (Bool, Maybe Handle)
isEncrypted "" = return (False, Nothing)
isEncrypted path = do
    current <- doesFileExist path
    if isPDF path && current
        then do
            (_, Just hout, _, _) <- createProcess (proc "exiftool" ["-s", "-T", "-Encrypted", path]) { std_out = CreatePipe }
            out <- hGetContents hout
            return (length out > 1, Just hout)
        else return (False, Nothing)
