{-# LANGUAGE OverloadedStrings #-}

module Bootstrap.Components where

import           Bootstrap.Util
import           Snap
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml as X
import           Data.Maybe (fromMaybe)
import           Data.Monoid

dividerSplice :: MonadSnap m => Splice m
dividerSplice = getParamNode <$$>
  mkElement' "li" (const []) []
    [ ( "vertical" , Just . flip (maybe id (<>)) "divider" . ("vertical-" <$) ) ]
    [] []

-- Dropdown {{{

dropdownSplice :: MonadSnap m => Splice m
dropdownSplice = getParamNode <$$>
  mkElement "ul"
    [ "dropdown-menu" ]
    [ ( "pull-right" , ("pull-right" <$) ) ]
    [ ( "role" , "menu" ) ] []

entrySplice :: MonadSnap m => Splice m
entrySplice = do
  node <- getParamNode
  let aNodes = mkElement "a"
        [] []
        [ ( "tabindex" , "-1" ) ]
        [ ( "href"     , (\mhr -> Just ("href",fromMaybe "#" mhr)) )
        , ( "active"   , (("active","") <$) )
        ]
        node
  let liNodes = mkElement' "li" (const aNodes)
        [] [] []
        [ ( "disabled" , (("disabled","") <$) ) ]
        node
  return liNodes

-- }}}

-- Button {{{

btnSplice :: MonadSnap m => Splice m
btnSplice = getParamNode <$$>
  mkElement "button"
    [ "btn" ]
    [ ( "size"  , fmap ("btn-" <>) )
    , ( "color" , fmap ("btn-" <>) )
    ]
    [] []

btngroupSplice :: MonadSnap m => Splice m
btngroupSplice = getParamNode <$$>
  mkElement "div"
    [ "btn-group" ]
    [ ( "vertical" , ("btn-group-vertical" <$) ) ]
    [] []

btntoolbarSplice :: MonadSnap m => Splice m
btntoolbarSplice = getParamNode <$$>
  mkElement "div" [ "btn-toolbar" ] [] [] []

-- }}}

-- Button Dropdown {{{

btndropupSplice :: MonadSnap m => Splice m
btndropupSplice = do
  node <- getParamNode
  let ulNodes = mkElement "ul"
                  [ "dropdown-menu" ] []
                  [] []
                  node
  let dNodes = mkElement' "div" (const ulNodes)
                 [ "btn-group" , "dropup" ] []
                 [] []
                 node
  return dNodes

btndropdownSplice :: MonadSnap m => Splice m
btndropdownSplice = do
  node <- getParamNode
  let mtxt = X.getAttribute "label" node
  let aNodes = mkElement' "a"
        (const
          [ X.TextNode (fromMaybe "" mtxt)
          , X.Element "span" [("class","caret")] []
          ])
        [ "btn" , "dropdown-toggle" ]
        []
        [ ( "data-toggle" , "dropdown" )
        , ( "href"        , "#" )
        ]
        []
        node
  let dNodes = mkElement' "div" (aNodes ++)
        [ "btn-group" ]
        [ ( "size" , fmap ("btn-" <>) ) ]
        [] []
        node
  return dNodes
        
-- }}}

-- Nav {{{

navSplice :: MonadSnap m => Splice m
navSplice = getParamNode <$$>
  mkElement "ul"
    [ "nav" ]
    [ ( "type"    , fmap ("nav-" <>) )
    , ( "pull"    , fmap ("pull-" <>) )
    , ( "stacked" , ("nav-stacked" <$) )
    , ( "list"    , ("nav-list" <$) )
    ]
    [] []

navheaderSplice :: MonadSnap m => Splice m
navheaderSplice = getParamNode <$$>
  mkElement "li"
    [ "nav-header" ]
    [] [] []

navbarSplice :: MonadSnap m => Splice m
navbarSplice = getParamNode <$$>
  mkElement "div"
    [ "navbar" ]
    [] [] []

navbarheaderSplice :: MonadSnap m => Splice m
navbarheaderSplice = getParamNode <$$>
  mkElement "div"
    [ "navbar-header" ]
    [] [] []

navbarcollapseSplice :: MonadSnap m => Splice m
navbarcollapseSplice = do
  node <- getParamNode
  let ulNodes = mkElement "ul"
                  [ "nav" , "navbar-nav" ]
                  [] [] []
                  node
  let dNodes = mkElement' "div" (const ulNodes)
                 [ "collapse" , "navbar-collapse" ]
                 [] [] []
                 node
  return dNodes

tabbableSplice :: MonadSnap m => Splice m
tabbableSplice = getParamNode <$$>
  mkElement "div"
    [ "tabbable" ]
    [] [] []

brandSplice :: MonadSnap m => Splice m
brandSplice = getParamNode <$$>
  mkElement "a"
    [ "navbar-brand" ]
    []
    [] 
    [ ( "href" , (\mhr -> Just ("href",fromMaybe "#" mhr)) ) ]

navbarformSplice :: MonadSnap m => Splice m
navbarformSplice = getParamNode <$$>
  mkElement "form"
    [ "navbar-form" ]
    [ ( "pull" , fmap ("pull-" <>) ) ]
    [] []

-- }}}

bootstrapComponentSplices :: MonadSnap m => Splices (Splice m)
bootstrapComponentSplices = do
  "divider"         ## dividerSplice
  "dropdown"        ## dropdownSplice
  "entry"           ## entrySplice
  "btn"             ## btnSplice
  "btngroup"        ## btngroupSplice
  "btntoolbar"      ## btntoolbarSplice
  "btndropup"       ## btndropupSplice
  "btndropdown"     ## btndropdownSplice
  "nav"             ## navSplice
  "nav-header"      ## navheaderSplice
  "navbar"          ## navbarSplice
  "navbar-header"   ## navbarheaderSplice
  "navbar-collapse" ## navbarcollapseSplice
  "tabbable"        ## tabbableSplice
  "brand"           ## brandSplice
  "navbar-form"     ## navbarformSplice

