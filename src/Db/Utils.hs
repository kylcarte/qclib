{-# LANGUAGE OverloadedStrings #-}

module Db.Utils where

import           Snap
import           Database.SQLite.Simple
import qualified Data.Text as T
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Monoid
import           Data.Int

tableExists :: Connection -> T.Text -> IO Bool
tableExists conn tblName = do
  r <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r :: [Only String] of
    [_] -> return True
    _   -> return False

createTable :: Connection -> T.Text -> [(T.Text,T.Text)] -> IO ()
createTable conn tblName tblFields = do
  created <- tableExists conn tblName
  unless created $ execute_ conn $
    mkQuery
      [ "CREATE TABLE"
      , tblName
      , wrapPar $ commas $ map (\(n,t) -> n <> " " <> t) tblFields
      ]

getRows :: FromRow a => Connection -> T.Text -> [T.Text] -> IO [a]
getRows conn tblName tblFlds = query_ conn $ mkQuery
  [ "SELECT"
  , commas tblFlds
  , "FROM"
  , tblName
  ]

newRow :: ToRow a => Connection -> T.Text -> [T.Text] -> a -> IO Int64
newRow conn tblName tblFlds r = do
  execute conn
    (mkQuery
      [ "INSERT INTO"
      , tblName
      , wrapPar $ commas tblFlds
      , "VALUES"
      , wrapPar $ commas $ replicate (length tblFlds) "?"
      ])
    r
  lastInsertRowId conn

updateRow :: ToRow a => Connection -> T.Text -> [T.Text] -> a -> T.Text -> Int64 -> IO Int64
updateRow conn tblName tblFlds r idFld idVal = do
  execute conn
    (mkQuery
      [ "UPDATE"
      , tblName
      , "SET"
      , commas $ map (<> " = ?") tblFlds
      , "WHERE"
      , idFld <> " = ?"
      ])
    (r :. Only idVal)
  return idVal

deleteRow :: FromRow a => Connection -> T.Text -> T.Text -> Int64 -> IO [a]
deleteRow conn tblName idFld idVal = do
  query conn
    (mkQuery
      [ "DELETE FROM"
      , tblName
      , "WHERE"
      , idFld <> " = ?"
      ])
    (Only idVal)


type Field = T.Text

fldTimestamp :: Field
fldTimestamp = "timestamp"

primaryKeyDBType :: T.Text
primaryKeyDBType = "INTEGER PRIMARY KEY"

timestampDBType :: T.Text
timestampDBType = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL"

noIdError :: String -> a
noIdError typ = error $ "ToJSON: " ++ typ ++ " must have Id"

wrapPar :: T.Text -> T.Text
wrapPar = T.cons '(' . flip T.snoc ')'

commas :: [T.Text] -> T.Text
commas = T.intercalate ","

commaSpaces :: [T.Text] -> T.Text
commaSpaces = T.intercalate ", "

mkQuery :: [T.Text] -> Query
mkQuery = Query . T.unwords

(.:#) :: (Num a) => Object -> T.Text -> Parser a
v .:# f = do
  x <- v .: f
  if all isDigit x
    then return $ fromInteger $ read x
    else fail "Expected an integer-representing string"

newtype Id = Id Int64 deriving (Show)

instance FromJSON Id where
  parseJSON (Object v) = Id
    <$> v .: "id"
  parseJSON _ = mzero

instance ToJSON Id where
  toJSON (Id i) = object
    [ "id" .= i
    ]

instance FromRow Id where
  fromRow = Id <$> field

