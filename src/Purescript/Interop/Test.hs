{-# LANGUAGE TemplateHaskell #-}

module Purescript.Interop.Test where

import Purescript.Interop

newtype Session = Session { unSession :: String }

mkExports Nothing [ ''Session ]
