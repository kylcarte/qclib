{-# LANGUAGE OverloadedStrings #-}

module Bootstrap.Layout where

import           Bootstrap.Util
import           Snap
import           Heist
import           Heist.Interpreted
import qualified Data.Text as T
import qualified Text.XmlHtml as X
import           Data.Monoid

-- Row {{{

rowSplice :: MonadSnap m => Splice m
rowSplice = getParamNode <$$>
  mkElement "div"
    [ "row" ]
    [] [] []

-- }}}

-- Col {{{

colSplice :: MonadSnap m => Splice m
colSplice = do
  node  <- getParamNode
  let mcols = X.getAttribute "width" node
  let moff  = X.getAttribute "offset" node
  classes <- colClasses12Cols mcols moff
  return $ mkElement "div"
    classes
    [] [] []
    node

colClasses12Cols :: Monad m => Maybe T.Text -> Maybe T.Text -> m [T.Text]
colClasses12Cols mc mo = do
  mcols <- viewInt mc
  moff  <- viewInt mo
  (width,offset) <- case (mcols,moff) of
    -- width=x offset=y
    (Just (c,ct),Just (o,ot))
      -- offset is non-zero
      | and [c >= 0,o > 0,c + o <= 12]
        -> return (ct,Just ot)
      -- offset is zero
      | and [c >= 0,o == 0,c <= 12]
        -> return (ct,Nothing)
      | otherwise
        -> fail $ "Out of 12 column range: width " ++ T.unpack ct
               ++ ", offset " ++ T.unpack ot
    -- width=x
    (Just (c,ct),_)
      | and [c >= 0,c <= 12]
        -> return (ct,Nothing)
      | otherwise
        -> fail $ "Out of 12 column range: width " ++ T.unpack ct
    -- offset=y
    (_,Just (o,ot))
      -- offset is non-zero
      | and [o > 0, o <= 12]
        -> return (T.pack $ show (12 - o),Just ot)
      -- offset is zero
      | o == 0
        -> return ("12",Nothing)
      | otherwise
        -> fail $ "Out of 12 column range: offset " ++ T.unpack ot
    -- <empty>
    _ -> return ("12",Nothing)
  return $ mAttr ("col-md-offset-" <>) offset [ "col-md-" <> width ]

-- }}}

-- Container {{{

containerSplice :: MonadSnap m => Splice m
containerSplice = getParamNode <$$>
  mkElement "div"
    [ "container" ]
    [] [] []

-- }}}

bootstrapLayoutSplices :: MonadSnap m => Splices (Splice m)
bootstrapLayoutSplices = do
  "row"       ## rowSplice
  "col"       ## colSplice
  "container" ## containerSplice

