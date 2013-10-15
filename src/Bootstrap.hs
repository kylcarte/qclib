
module Bootstrap where

import Snap
import Heist
import Heist.Interpreted
import Bootstrap.Layout
import Bootstrap.Components

import Data.Monoid

bootstrapSplices :: MonadSnap m => Splices (Splice m)
bootstrapSplices = bootstrapLayoutSplices <> bootstrapComponentSplices

