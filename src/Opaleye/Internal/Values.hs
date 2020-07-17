{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Opaleye.Internal.Values where

import           Opaleye.Internal.Column (Column(Column))
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PGTypes
import qualified Opaleye.SqlTypes

import           Data.Profunctor (Profunctor, dimap, rmap)
import           Data.Profunctor.Product (ProductProfunctor)
import qualified Data.Profunctor.Product as PP
import           Data.Profunctor.Product.Default (Default, def)

import           Control.Applicative (Applicative, pure, (<*>))

-- FIXME: We don't currently handle the case of zero columns.  Need to
-- emit a dummy column and data.
valuesU :: U.Unpackspec columns columns'
        -> Valuesspec columns columns'
        -> [columns]
        -> ((), T.Tag) -> (columns', PQ.PrimQuery, T.Tag)
valuesU unpack valuesspec rows ((), t) = (newColumns, primQ', T.next t)
  where runRow row = valuesRow
           where (_, valuesRow) =
                   PM.run (U.runUnpackspec unpack extractValuesEntry row)

        (newColumns, valuesPEs_nulls) =
          PM.run (runValuesspec valuesspec (extractValuesField t))

        valuesPEs = map fst valuesPEs_nulls

        values :: [[HPQ.PrimExpr]]
        values = map runRow rows

        primQ' = case values of
          []      -> PQ.Empty ()
          values' -> PQ.Values valuesPEs values'

-- We don't actually use the return value of this.  It might be better
-- to come up with another Applicative instance for specifically doing
-- what we need.
extractValuesEntry :: HPQ.PrimExpr -> PM.PM [HPQ.PrimExpr] HPQ.PrimExpr
extractValuesEntry pe = do
  PM.write pe
  return pe

extractValuesField :: T.Tag -> primExpr
                   -> PM.PM [(HPQ.Symbol, primExpr)] HPQ.PrimExpr
extractValuesField = PM.extractAttr "values"

newtype Valuesspec columns columns' =
  Valuesspec (PM.PackMap () HPQ.PrimExpr () columns')

runValuesspec :: Applicative f => Valuesspec columns columns'
              -> (() -> f HPQ.PrimExpr) -> f columns'
runValuesspec (Valuesspec v) f = PM.traversePM v f ()

-- For 0.7 put an `IsSqlType a` constraint on here, so that we can
-- later use it without breaking the API
instance Default Valuesspec (Column a) (Column a) where
  def = Valuesspec (PM.iso id Column)

valuesUSafe :: U.Unpackspec columns columns'
            -> ValuesspecSafe columns columns'
            -> [columns]
            -> ((), T.Tag) -> (columns', PQ.PrimQuery, T.Tag)
valuesUSafe unpack valuesspec rows ((), t) = (newColumns, primQ', T.next t)
  where runRow row = valuesRow
           where (_, valuesRow) =
                   PM.run (U.runUnpackspec unpack extractValuesEntry row)

        (runVs, anyFields) = runValuesspecSafe valuesspec (extractValuesField t)
        (newColumns, valuesPEs_nulls) = PM.run runVs

        valuesPEs = map fst valuesPEs_nulls
        nulls = map snd valuesPEs_nulls

        yieldNoRows :: PQ.PrimQuery -> PQ.PrimQuery
        yieldNoRows = PQ.restrict (HPQ.ConstExpr (HPQ.BoolLit False))

        zero = HPQ.ConstExpr (HPQ.IntegerLit 0)

        nullInt = HPQ.CastExpr (Opaleye.Internal.PGTypes.showSqlType
                                  (Nothing :: Maybe Opaleye.SqlTypes.SqlInt4))
                               (HPQ.ConstExpr HPQ.NullLit)

        (values, wrap) = case (null rows, not anyFields) of
          (True, True)   -> ([[nullInt]], yieldNoRows)
          (True, False)  -> ([nulls], yieldNoRows)
          (False, True)  -> (map (const [zero]) rows, id)
          (False, False) -> (map runRow rows, id)

        primQ' = wrap (PQ.Values valuesPEs values)

data ValuesspecSafe columns columns' =
  ValuesspecSafe {
    vsMapper    :: PM.PackMap HPQ.PrimExpr HPQ.PrimExpr () columns'
  , vsAnyFields :: Bool
  }

runValuesspecSafe :: Applicative f
                  => ValuesspecSafe columns columns'
                  -> (HPQ.PrimExpr -> f HPQ.PrimExpr)
                  -> (f columns', Bool)
runValuesspecSafe (ValuesspecSafe v a) f = (PM.traversePM v f (), a)

instance Opaleye.Internal.PGTypes.IsSqlType a
  => Default ValuesspecSafe (Column a) (Column a) where
  def = def_
    where def_ = ValuesspecSafe(PM.PackMap (\f () -> fmap Column (f null_)))
                               True

          null_ = HPQ.CastExpr (Opaleye.Internal.PGTypes.showSqlType sqlType)
                               (HPQ.ConstExpr HPQ.NullLit)

          sqlType = columnProxy def_
          columnProxy :: f (Column sqlType) -> Maybe sqlType
          columnProxy _ = Nothing


-- {

-- Boilerplate instance definitions.  Theoretically, these are derivable.

instance Functor (Valuesspec a) where
  fmap f (Valuesspec g) = Valuesspec (fmap f g)

instance Applicative (Valuesspec a) where
  pure = Valuesspec . pure
  Valuesspec f <*> Valuesspec x = Valuesspec (f <*> x)

instance Profunctor Valuesspec where
  dimap _ g (Valuesspec q) = Valuesspec (rmap g q)

instance ProductProfunctor Valuesspec where
  purePP = pure
  (****) = (<*>)

instance Functor (ValuesspecSafe a) where
  fmap f (ValuesspecSafe g a) = ValuesspecSafe (fmap f g) a

instance Applicative (ValuesspecSafe a) where
  pure a = ValuesspecSafe (pure a) False
  ValuesspecSafe f af <*> ValuesspecSafe x ax =
    ValuesspecSafe (f <*> x) (af || ax)

instance Profunctor ValuesspecSafe where
  dimap _ g (ValuesspecSafe q a) = ValuesspecSafe (rmap g q) a

instance ProductProfunctor ValuesspecSafe where
  purePP = pure
  (****) = (<*>)

-- }
