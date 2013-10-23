{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent (withMVar)
import           Control.Lens
import           Control.Monad.Trans (liftIO)
import           Data.ByteString (ByteString)
import           Data.Monoid
import qualified Data.Text as T
------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.SqliteSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.SqliteSimple
import           Snap.Util.FileServe
import           Snap.Extras.JSON
import           Snap.Elm
import           Heist
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import           Bootstrap
import           Db
import           Db.Utils
import qualified Data.Patron as P

type H     = Handler App App
type AuthH = Handler App (AuthManager App)

-- Login {{{

-- | Render login form
handleLogin :: Maybe T.Text -> AuthH ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


-- | Handle login submit
handleLoginSubmit :: AuthH ()
handleLoginSubmit = loginUser "login" "password" (Just "remember-me")
    (const $ handleLogin err)
    (redirect "/")
  where
    err = Just "Unknown user or password"

-- }}}

-- Logout {{{

-- | Logs out and redirects the user to the site index.
handleLogout :: AuthH ()
handleLogout = logout >> redirect "/"

-- }}}

-- New User {{{

-- | Handle new user form submit
handleNewUser :: AuthH ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

-- }}}

-- | The application's routes.
routes :: ElmOptions -> [(ByteString, H ())]
routes opts =
  [ ("/"           , redirect "/static")
  , ("/login"      , with auth handleLoginSubmit)
  , ("/logout"     , with auth handleLogout)
  , ("/new_user"   , with auth handleNewUser)
  , ("/api/patron" , handlePatrons)
  , ("/static"     , serveDirectory "static")
  , serveElmDirectory opts "/elm"
  , serveElmRuntime opts
  ]

handlePatrons :: H ()
handlePatrons =
  method GET getPatrons   <|>
  method POST savePatron  <|>
  method DELETE delPatron
  where
  getPatrons = do
    ps <- withSqlite P.getPatrons
    writeJSON ps
  savePatron = do
    mp <- getJSON
    liftIO $ do
      putStrLn "savePatron:"
      print mp
    either (const $ return ()) persist mp
    where
    persist p = do
      p' <- withSqlite $ \conn -> P.savePatron conn p
      writeJSON p'
  delPatron = do
    mi <- getJSON
    liftIO $ do
      putStrLn "delPatron:"
      print (mi :: Either String Id)
    either (const $ return ()) remove mi
    where
    remove (Id i) = do
      ps <- withSqlite $ \conn -> P.deletePatron conn i
      writeJSON ps


myHeistConfig :: HeistConfig H
myHeistConfig = mempty
  { hcInterpretedSplices = bootstrapSplices
  }

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    elm <- setElmSourcePath "static/elm" <$>
           setElmVerbose True <$>
           defaultElmOptions
    addRoutes $ routes elm

    h <- nestSnaplet "" heist $ heistInit "templates"
    addConfig h myHeistConfig

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices h auth

    let conn = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar conn createAllTables

    return $ App h s d a

