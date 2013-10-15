{-# LANGUAGE OverloadedStrings #-}

module Bootstrap.Util where

import           Data.Char (isDigit)
import           Data.Monoid
import qualified Data.Text as T
import qualified Text.XmlHtml as X

whenOn :: Maybe a -> (a -> b -> b) -> b -> b
whenOn ma f = case ma of
  Just a -> f a
  _ -> id

mAttr :: (a -> b) -> Maybe a -> [b] -> [b]
mAttr f ma = case ma of
  Just a -> (f a :)
  _      -> id

mAttr' :: b -> Maybe a -> [b] -> [b]
mAttr' b = mAttr (const b)

maybeSuffix :: Maybe T.Text -> T.Text -> T.Text
maybeSuffix ms t = case ms of
  Just s  -> t <> "-" <> s
  Nothing -> t

viewInt :: Monad m => Maybe T.Text -> m (Maybe (Int,T.Text))
viewInt mt = case mt of
  Nothing -> return Nothing
  Just t | T.all isDigit t
           -> return $ Just (read $ T.unpack t,t)
         | otherwise
           -> fail $ "Not an Int: " ++ T.unpack t

mkElement :: T.Text
  -> [T.Text]
  -> [(T.Text,Maybe T.Text -> Maybe T.Text)]
  -> [(T.Text,T.Text)]
  -> [(T.Text,Maybe T.Text -> Maybe (T.Text,T.Text))]
  -> X.Node
  -> [X.Node]
mkElement el = mkElement' el id

mkElement' :: T.Text
  -> ([X.Node] -> [X.Node])
  -> [T.Text]
  -> [(T.Text,Maybe T.Text -> Maybe T.Text)]
  -> [(T.Text,T.Text)]
  -> [(T.Text,Maybe T.Text -> Maybe (T.Text,T.Text))]
  -> X.Node
  -> [X.Node]
mkElement' el nodesBldr cs optClasses as optAttrs node =
  [X.Element el (allAttrs ++ existingAttrs) $ nodesBldr nodes]
  where
  gatherTag (tagAttr,tagF) =
    case tagF $ X.getAttribute tagAttr node of
      Just res  -> (res :)
      Nothing -> id
  attrs   = foldr gatherTag as optAttrs
  classes = foldr gatherTag cs optClasses
  nodes = X.childNodes node
  allAttrs = if null classes then attrs else ("class",T.unwords classes):attrs
  attrTags = map fst optClasses ++ map fst optAttrs
  existingAttrs = filter (\(x,_) -> not (x `elem` attrTags)) $ X.elementAttrs node

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip fmap

