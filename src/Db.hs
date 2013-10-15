{-# LANGUAGE OverloadedStrings #-}

module Db where

import           Database.SQLite.Simple
------------------------------------------------------------------------------
import           Data.Book
import           Data.Checkout
import           Data.Patron

createAllTables :: Connection -> IO ()
createAllTables conn = do
  createPatronTable   conn
  createBookTable     conn
  createCheckoutTable conn

