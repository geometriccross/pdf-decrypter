module Decrypt 
    (   isPDF, 
        isEncrypted
    ) where

import           System.Directory
import           System.IO
import           System.Process

-- >>> isPDF "test.pdf"
-- True

isPDF :: String -> Bool
isPDF str
    | str `elem` [".pdf", ".PDF"] = True
    | otherwise = False

-- >>> isEncrypted "invailde/path/to/file.pdf"
-- (False, Nothing)
--

isEncrypted :: String -> IO (Bool, Maybe Handle)
isEncrypted "" = return (False, Nothing)
isEncrypted path = do
    current <- doesPathExist path
    exist <- doesFileExist path
    if current && exist
        then do
            (_, Just hout, _, _) <- createProcess (proc "exiftool" ["-s", "-T", "-Encrypted", path]) { std_out = CreatePipe }
            out <- hGetContents hout
            return (length out > 1, Just hout)
        else return (False, Nothing)
