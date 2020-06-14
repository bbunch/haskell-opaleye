{-# LANGUAGE FlexibleContexts #-}

module Opaleye.ToFields (C.ToFields, module Opaleye.ToFields) where

import qualified Opaleye.Constant as C
import qualified Data.Profunctor.Product.Default as D

toFieldsExplicit :: C.ToFields haskells fields -> haskells -> fields
toFieldsExplicit = C.constantExplicit

-- | 'toFields' provides a convenient typeclass wrapper around the
-- 'Opaleye.Field.Field_' creation functions in "Opaleye.SqlTypes".  Besides
-- convenience it doesn't provide any additional functionality.
--
-- It can be used with functions like 'Opaleye.Manipulation.runInsert'
-- to insert custom Haskell types into the database.
-- The following is an example of a function for inserting custom types.
--
-- @
--   customInsert
--      :: ( 'D.Default' 'ToFields' haskells fields )
--      => Connection
--      -> 'Opaleye.Table' fields fields'
--      -> haskells
--      -> IO Int64
--   customInsert conn table haskells = 'Opaleye.Manipulation.runInsert' conn table $ 'toFields' haskells
-- @
--
-- In order to use this function with your custom types, you need to define an
-- instance of 'D.Default' 'ToFields' for your custom types.
toFields :: D.Default C.ToFields haskells fields => haskells -> fields
toFields = C.toFields
