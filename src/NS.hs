{-# LANGUAGE FlexibleContexts #-}

module NS where

import Data.Proxy
import GHC.Exts (Constraint)
import NP

data NS :: (k -> *) -> [k] -> * where
