{-# LANGUAGE FlexibleContexts #-}

module Opaleye.ToFields (C.ToFields, module Opaleye.ToFields) where

import qualified Opaleye.Constant as C
import qualified Data.Profunctor.Product.Default as D

toFieldsExplicit :: C.ToFields haskells fields -> haskells -> fields
toFieldsExplicit = C.constantExplicit

toFields :: D.Default C.ToFields haskells fields => haskells -> fields
toFields = C.toFields
