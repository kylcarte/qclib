{-# LANGUAGE OverloadedStrings #-}

module Data.Patron where

import           Snap
import           Database.SQLite.Simple
import qualified Data.Text as T
import           Data.Aeson
import           Data.Int
------------------------------------------------------------------------------
import           Db.Utils


-- | A Patron of the library, with contact info.
data Patron = Patron
  { pId           :: Maybe Int64
  , pNumber       :: Int64
  } deriving (Eq,Show)

instance FromRow Patron where
  fromRow = Patron
    <$> field -- ^ pId
    <*> field -- ^ pNumber

-- Schema --------------------------------------------------------------------

fldPId, fldPNumber :: Field
fldPId           = "id"
fldPNumber       = "number"

patronTableName :: T.Text
patronTableName = "patrons"

patronTableFields :: [(T.Text,T.Text)]
patronTableFields =
  [ ( fldPId       , "INTEGER PRIMARY KEY" )
  , ( fldPNumber   , "INTEGER NOT NULL"    )
  , ( fldTimestamp , timestampDBType       )
  ]

-- DB Ops --------------------------------------------------------------------

createPatronTable :: Connection -> IO ()
createPatronTable conn = createTable conn patronTableName patronTableFields

getPatrons :: Connection -> IO [Patron]
getPatrons conn = getRows conn patronTableName
  [ fldPId , fldPNumber ]

savePatron :: Connection -> Patron -> IO Patron
savePatron conn pat = do
  pid <- maybe newPatron updatePatron (pId pat)
  return pat { pId = Just pid }
  where
    flds = [ fldPNumber ]
    vals = ( Only $ pNumber pat )
    newPatron = newRow conn patronTableName flds vals
    updatePatron = updateRow conn patronTableName flds vals fldPId

deletePatron :: Connection -> Int64 -> IO [Patron]
deletePatron conn = deleteRow conn patronTableName fldPId

-- JSON ----------------------------------------------------------------------

instance FromJSON Patron where
  parseJSON (Object v) = Patron
    <$> v .:? fldPId
    <*> v .:# fldPNumber
  parseJSON _ = mzero

instance ToJSON Patron where
  toJSON pat = object
    [ fldPId     .= pId_    pat
    , fldPNumber .= pNumber pat
    ]
    where
    pId_ = maybe (noIdError "Patron") id . pId

