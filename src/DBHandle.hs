{-# LANGUAGE OverloadedStrings #-}

module DBHandle where

import Database.SQLite.Simple
    ( execute, open, Only(Only), Connection )

import           Control.Monad.Free
import           Decrypt

type Password = String

data DBProcF next = Open (FilePath -> IO Connection) next
                | Insert (Connection -> IO ()) next
                | Close (Connection -> IO ())


type DBProc = Free DBProcF

instance Functor DBProcF where
    fmap f (Open path next)  = Open (open path) (f next)
    fmap f (Insert ins next) = Insert ins (f next)
    fmap f (Close con)       = Close con

insertPassword :: Connection -> Password -> IO ()
insertPassword conn = execute conn "INSERT INTO passwords (password_text) VALUES (?)" . Only

insertFilePath :: Connection -> FilePath -> IO ()
insertFilePath conn = execute conn "INSERT INTO files (file_path) VALUES (?)" . Only
