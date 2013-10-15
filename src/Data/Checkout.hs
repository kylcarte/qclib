{-# LANGUAGE OverloadedStrings #-}

module Data.Checkout where

import           Snap
import           Database.SQLite.Simple
import qualified Data.Text as T
import           Data.Aeson
import           Data.Int
------------------------------------------------------------------------------
import           Db.Utils

-- | A Checkout in the library.
data Checkout = Checkout
  { cId           :: Maybe Int64
  , cPatronId     :: Int64
  , cBookId       :: Int64
  } deriving (Eq,Show)

instance FromRow Checkout where
  fromRow = Checkout
    <$> field -- ^ cId
    <*> field -- ^ cPatronId
    <*> field -- ^ cBookId

-- Schema ----------------------------------------------------------------------

fldCId, fldCPatronId, fldCBookId :: Field
fldCId       = "id"
fldCPatronId = "patronid"
fldCBookId   = "bookid"

checkoutTableName :: T.Text
checkoutTableName = "checkouts"

checkoutTableFields :: [(T.Text,T.Text)]
checkoutTableFields =
  [ ( fldCId       , primaryKeyDBType   )
  , ( fldCPatronId , "INTEGER NOT NULL" )
  , ( fldCBookId   , "INTEGER NOT NULL" )
  , ( fldTimestamp , timestampDBType    )
  ]

-- DB Ops ----------------------------------------------------------------------

createCheckoutTable :: Connection -> IO ()
createCheckoutTable conn = createTable conn checkoutTableName checkoutTableFields

getCheckouts :: Connection -> IO [Checkout]
getCheckouts conn = getRows conn checkoutTableName
  [ fldCId , fldCPatronId , fldCBookId ]

saveCheckout :: Connection -> Checkout -> IO Checkout
saveCheckout conn co = do
  cid <- maybe newCheckout updateCheckout (cId co)
  return co { cId = Just cid }
  where
    flds = [ fldCPatronId , fldCBookId ]
    vals = ( cPatronId co , cBookId co )
    newCheckout = newRow conn checkoutTableName flds vals
    updateCheckout = updateRow conn checkoutTableName flds vals fldCId

-- JSON ------------------------------------------------------------------------

instance FromJSON Checkout where
  parseJSON (Object v) = Checkout
    <$> v .:? fldCId
    <*> v .:  fldCPatronId
    <*> v .:  fldCBookId
  parseJSON _ = mzero

instance ToJSON Checkout where
  toJSON co = object
    [ fldCId       .= cId_      co
    , fldCPatronId .= cPatronId co
    , fldCBookId   .= cBookId   co
    ]
    where
    cId_ = maybe (noIdError "Checkout") id . cId

