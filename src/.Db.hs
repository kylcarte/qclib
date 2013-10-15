{-# LANGUAGE OverloadedStrings #-}

module Db where

import           Snap
import           Snap.Snaplet.Auth
import           Database.SQLite.Simple
import qualified Data.Text as T
import           Data.Aeson
import           Data.Maybe (fromJust)
import           Data.Monoid
import           Data.Int

-- User {{{

-- | A user of the library GUI, with either
--   admin permissions or volunteer permissions.
data User = User
  { uId       :: Maybe Int64
  , uUserId   :: UserId
  , uLogin    :: T.Text
  , uFstName  :: T.Text
  , uLstName  :: T.Text
  , uIsAdmin  :: Bool
  }

fldUId, fldUUserId, fldULogin, fldUFstName, 
  fldULstName, fldUIsAdmin :: Field
fldUId      = "id"
fldUUserId  = "userid"
fldULogin   = "login"
fldUFstName = "firstname"
fldULstName = "lastname"
fldUIsAdmin = "isadmin"

instance FromJSON User where
  parseJSON (Object v) = User
    <$> v .: fldUId
    <*> v .: fldUUserId
    <*> v .: fldULogin
    <*> v .: fldUFstName
    <*> v .: fldULstName
    <*> v .: fldUIsAdmin
  parseJSON _ = mzero

instance ToJSON User where
  toJSON user = object
    [ fldUId      .= uId_
    , fldUUserId  .= unUid (uUserId user)
    , fldULogin   .= uLogin   user
    , fldUFstName .= uFstName user
    , fldULstName .= uLstName user
    , fldUIsAdmin .= uIsAdmin user
    ]
    where
    uId_ = case uId user of
             Nothing -> noIdError "User"
             Just i  -> i

instance FromRow User where
  fromRow = User
    <$> field              -- ^ uId
    <*> (UserId <$> field) -- ^ uUserId
    <*> field              -- ^ uLogin
    <*> field              -- ^ uFstName
    <*> field              -- ^ uLstName
    <*> field              -- ^ uIsAdmin

-- }}}

-- Patron {{{

-- | A Patron of the library, with contact info.
data Patron = Patron
  { pId           :: Maybe Int64
  , pFstName      :: T.Text
  , pLstName      :: T.Text
  , pPhone        :: T.Text
  , pEmail        :: T.Text
  , pPrefersEmail :: Bool
  , pNumber       :: Int64
  , pHomeAddr1    :: Maybe T.Text
  , pHomeAddr2    :: Maybe T.Text
  , pCity         :: Maybe T.Text
  , pState        :: Maybe T.Text
  , pZip          :: Maybe T.Text
  } deriving (Eq,Show)

fldPId, fldPFstName, fldPLstName,
  fldPPhone, fldPEmail, fldPPrefersEmail, fldPNumber, 
  fldPHomeAddr1, fldPHomeAddr2,
  fldPCity, fldPState, fldPZip :: Field
fldPId           = "id"
fldPFstName      = "firstname"
fldPLstName      = "lastname"
fldPPhone        = "phone"
fldPEmail        = "email"
fldPPrefersEmail = "prefersemail"
fldPNumber       = "number"
fldPHomeAddr1    = "home1"
fldPHomeAddr2    = "home2"
fldPCity         = "city"
fldPState        = "state"
fldPZip          = "zip"

instance FromJSON Patron where
  parseJSON (Object v) = Patron
    <$> v .:? fldPId
    <*> v .:  fldPFstName
    <*> v .:  fldPLstName
    <*> v .:  fldPPhone
    <*> v .:  fldPEmail
    <*> v .:  fldPPrefersEmail
    <*> v .:  fldPNumber
    <*> v .:? fldPHomeAddr1
    <*> v .:? fldPHomeAddr2
    <*> v .:? fldPCity
    <*> v .:? fldPState
    <*> v .:? fldPZip
  parseJSON _ = mzero

instance ToJSON Patron where
  toJSON pat = object
    [ fldPId           .= pId_
    , fldPFstName      .= pFstName      pat
    , fldPLstName      .= pLstName      pat
    , fldPPhone        .= pPhone        pat
    , fldPEmail        .= pEmail        pat
    , fldPPrefersEmail .= pPrefersEmail pat
    , fldPNumber       .= pNumber       pat
    , fldPHomeAddr1    .= pHomeAddr1    pat
    , fldPHomeAddr2    .= pHomeAddr2    pat
    , fldPCity         .= pCity         pat
    , fldPState        .= pState        pat
    , fldPZip          .= pZip          pat
    ]
    where
    pId_ = case pId pat of
             Nothing -> noIdError "Patron"
             Just i  -> i

instance FromRow Patron where
  fromRow = Patron
    <$> field -- ^ pId
    <*> field -- ^ pFstName
    <*> field -- ^ pLstName
    <*> field -- ^ pPhone
    <*> field -- ^ pEmail
    <*> field -- ^ pPrefersEmail
    <*> field -- ^ pNumber
    <*> field -- ^ pHomeAddr1
    <*> field -- ^ pHomeAddr2
    <*> field -- ^ pCity
    <*> field -- ^ pState
    <*> field -- ^ pZip

createPatronTable :: Connection -> IO ()
createPatronTable conn = do
  created <- tableExists conn "patrons"
  unless created $ execute_ conn $
    Query $ "CREATE TABLE patrons "
     <> (wrapPar $ T.intercalate ", "
         [ fldPId           <> " INTEGER PRIMARY KEY"
         , fldPFstName      <> " TEXT NOT NULL"
         , fldPLstName      <> " TEXT NOT NULL"
         , fldPPhone        <> " TEXT NOT NULL"
         , fldPEmail        <> " TEXT NOT NULL"
         , fldPPrefersEmail <> " BOOLEAN NOT NULL"
         , fldPNumber       <> " INTEGER NOT NULL"
         , fldPHomeAddr1    <> " TEXT"
         , fldPHomeAddr2    <> " TEXT"
         , fldPCity         <> " TEXT"
         , fldPState        <> " TEXT"
         , fldPZip          <> " TEXT"
         , fldCreatedOn     <> createdOnDBType
         ])

getPatrons :: Connection -> IO [Patron]
getPatrons conn = query_ conn $ Query $ T.unwords
  [ "SELECT"
  , T.intercalate ","
    [ fldPId
    , fldPFstName
    , fldPLstName
    , fldPPhone
    , fldPEmail
    , fldPPrefersEmail
    , fldPNumber
    , fldPHomeAddr1
    , fldPHomeAddr2
    , fldPCity
    , fldPState
    , fldPZip
    ]
  , "FROM patrons"
  ]

savePatron :: Connection -> Patron -> IO Int64
savePatron conn pat = do
  maybe newPatron updatePatron (pId pat)
  where
    newPatron = do
      execute conn
        (Query $ T.unwords
          [ "INSERT INTO patrons"
          , wrapPar $ T.intercalate ","
            [ fldPFstName
            , fldPLstName
            , fldPPhone
            , fldPEmail
            , fldPPrefersEmail
            , fldPNumber
            , fldPHomeAddr1
            , fldPHomeAddr2
            , fldPCity
            , fldPState
            , fldPZip
            ]
          , "VALUES (?,?,?,?,?,?,?,?,?,?,?)"
          ])
        (  (pFstName pat, pLstName pat)
        :. (pPhone pat, pEmail pat, pPrefersEmail pat)
        :. (pNumber pat, pHomeAddr1 pat, pHomeAddr2 pat)
        :. (pCity pat, pState pat, pZip pat)
        )
      lastInsertRowId conn
    updatePatron pid = do
      execute conn
        (Query $ T.unwords
          [ "UPDATE patrons SET"
          , T.intercalate ", " $ map (<> " = ?")
            [ fldPFstName
            , fldPLstName
            , fldPPhone
            , fldPEmail
            , fldPPrefersEmail
            , fldPNumber
            , fldPHomeAddr1
            , fldPHomeAddr2
            , fldPCity
            , fldPState
            , fldPZip
            ]
          , "WHERE"
          , fldPId <> " = ?"
          ])
        (  (pFstName pat, pLstName pat)
        :. (pPhone pat, pEmail pat, pPrefersEmail pat)
        :. (pNumber pat, pHomeAddr1 pat, pHomeAddr2 pat)
        :. (pCity pat, pState pat, pZip pat)
        :. (Only pid)
        )
      return pid

-- }}}

-- Db {{{

tableExists :: Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r :: [Only String] of
    [_] -> return True
    _   -> return False

-- }}}

-- Helpers {{{

type Field = T.Text

fldCreatedOn :: Field
fldCreatedOn = "createdon"

createdOnDBType :: T.Text
createdOnDBType = " TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL"

noIdError :: String -> a
noIdError typ = error $ "ToJSON: " ++ typ ++ " must have Id"

wrapPar :: T.Text -> T.Text
wrapPar = T.cons '(' . flip T.snoc ')'

-- }}}

