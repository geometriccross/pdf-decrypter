module Decrypt (isEncrypted) where

import           System.Directory
import           System.IO
import           System.Process

type Password = String

-- >>> isEncrypted "invailde/path/to/file.pdf"
-- (False, Nothing)
--

isEncrypted :: FilePath -> IO (Bool, Maybe Handle)
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