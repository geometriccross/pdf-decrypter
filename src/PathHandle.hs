module PathHandle ( getPathRecursive ) where

import System.Directory
    ( doesDirectoryExist, getDirectoryContents )
import           System.FilePath.Posix

getPathRecursive :: FilePath -> IO [FilePath]
getPathRecursive "" = return []
getPathRecursive path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            contents <- getDirectoryContents path
            paths <- mapM (getPathRecursive . (path </>)) $ filter (`notElem` [".", ".."]) contents
            return $ concat paths
    else return [path]