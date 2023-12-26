{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module PathHandle ( getPath ) where

import           Control.Monad    (filterM)
import           Data.Foldable    (fold)
import           System.Directory (doesDirectoryExist, listDirectory, doesFileExist, getDirectoryContents)
import           System.IO.Unsafe (unsafeInterleaveIO)
import           System.FilePath

hasFile :: FilePath -> IO Bool
hasFile path = not . null <$> getDirectoryContents path

getPath :: (FilePath -> IO Bool) -- ^ Filepath filter
                    -> FilePath
                    -> IO [FilePath]
getPath p fp = do
    all' <- listDirectory fp
    all'' <- filterM p (mkRel <$> all')
    dirs <- filterM doesDirectoryExist all''
    case dirs of
        [] -> pure all''
        ds -> do
            next <- unsafeInterleaveIO $ foldMapA (getPath p) ds
            pure $ all'' ++ next
    where
        mkRel = (fp </>)
        foldMapA = (fmap fold .) . traverse
