{-# LANGUAGE OverloadedStrings #-}

module Data.Book where

import           Snap
import           Database.SQLite.Simple
import qualified Data.Text as T
import           Data.Aeson
import           Data.Int
------------------------------------------------------------------------------
import           Db.Utils

-- | A Book in the library.
data Book = Book
  { bId           :: Maybe Int64
  , bISBN         :: T.Text
  } deriving (Eq,Show)

instance FromRow Book where
  fromRow = Book
    <$> field -- ^ bId
    <*> field -- ^ bISBN

-- Schema ----------------------------------------------------------------------

fldBId, fldBISBN :: Field
fldBId   = "id"
fldBISBN = "isbn"

bookTableName :: T.Text
bookTableName = "books"

bookTableFields :: [(T.Text,T.Text)]
bookTableFields =
  [ ( fldBId       , primaryKeyDBType )
  , ( fldBISBN     , "TEXT NOT NULL"  )
  , ( fldTimestamp , timestampDBType  )
  ]

-- DB Ops ----------------------------------------------------------------------

createBookTable :: Connection -> IO ()
createBookTable conn = createTable conn bookTableName bookTableFields

getBooks :: Connection -> IO [Book]
getBooks conn = getRows conn bookTableName
  [ fldBId , fldBISBN ]

saveBook :: Connection -> Book -> IO Book
saveBook conn book = do
  bid <- maybe newBook updateBook (bId book)
  return book { bId = Just bid }
  where
    flds = [ fldBISBN ]
    vals = ( Only $ bISBN book )
    newBook = newRow conn bookTableName flds vals
    updateBook = updateRow conn bookTableName flds vals fldBId

-- JSON ------------------------------------------------------------------------

instance FromJSON Book where
  parseJSON (Object v) = Book
    <$> v .:? fldBId
    <*> v .:  fldBISBN
  parseJSON _ = mzero

instance ToJSON Book where
  toJSON book = object
    [ fldBId   .= bId_  book
    , fldBISBN .= bISBN book
    ]
    where
    bId_ = maybe (noIdError "Book") id . bId

