{-# LANGUAGE OverloadedStrings #-}

module Data.User where

import           Snap
import           Snap.Snaplet.Auth
import           Data.Aeson
import           Data.Int
------------------------------------------------------------------------------
import           Db.Utils

-- | A User of the Library GUI
data User = User
  { uId       :: Maybe Int64
  , uUserId   :: UserId
  }

fldUId, fldUUserId :: Field
fldUId      = "id"
fldUUserId  = "userid"

instance FromJSON User where
  parseJSON (Object v) = User
    <$> v .:? fldUId
    <*> v .:  fldUUserId
  parseJSON _ = mzero

instance ToJSON User where
  toJSON user = object
    [ fldUId      .= uId_     user
    , fldUUserId  .= uUserId_ user
    ]
    where
    uId_ = maybe (noIdError "User") id . uId
    uUserId_ = unUid . uUserId

