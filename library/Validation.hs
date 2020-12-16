{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}

module Validation
  ( -- * Data type
    Validation(Validation),
    pattern Failure,
    pattern Success,
    -- * Constructing validations
    -- * Functions on validations
    runValidation,
    andThen,
    -- * Misc. typeclass implementation functions
    apValidation,
    altValidation,
  ) where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData)
import Data.Data (Data)
import GHC.Generics (Generic)
import Prelude

#ifdef BIFUNCTORS
import Data.Bifoldable (Bifoldable (bifoldr))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Bifunctor (Bifunctor (bimap))
#endif

#ifdef SEMIGROUPOIDS
import Data.Functor.Alt (Alt((<!>)))
import Data.Functor.Apply (Apply((<.>)))
#endif

#ifdef LENS
-- Re-export lens-compatible typeclass instances.
import Validation.Lens as Lens ()
#endif

#ifdef OPTICS
-- Re-export optics-compatible typeclass instances.
import Validation.Optics as Optics ()
#endif

-------------------------------------------------------------------------------

-- | TODO(jkachmar): Documentation.
newtype Validation err result = Validation (Either err result)
  deriving stock (Data, Eq, Generic, Ord, Read, Show)
  deriving newtype (Functor, NFData)

-- | TODO(jkachmar): Documentation.
pattern Failure :: err -> Validation err result
pattern Failure err = Validation (Left err)

-- | TODO(jkachmar): Documentation.
pattern Success :: result -> Validation err result
pattern Success result = Validation (Right result)

{-# COMPLETE Failure, Success #-}

-------------------------------------------------------------------------------
-- 'base' typeclass instances

instance Semigroup err => Applicative (Validation err) where
  pure = Success
  (<*>) = apValidation
  {-# INLINE (<*>) #-}

-- Laws:
--
-- Success a <> (Success b <> Success c)
--   = (Success a <> Success b) <> Success c
--   = Success (a <> b <> c)
--
-- Success a <> (Failure e <> Success c)
--   = (Success a <> Failure e) <> Success c
--   = Failure e
--
-- Failure e0 <> (Failure e1 <> Success c)
--   = (Failure e0 <> Failure e1) <> Success c
--   = Failure (e0 <> e1)
instance (Semigroup err, Semigroup result) => Semigroup (Validation err result) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

-- Laws:
--
-- Sucess a <> Success mempty = Success a
-- Failure e <> Success mempty = Failure e
instance (Semigroup err, Monoid result) => Monoid (Validation err result) where
  mempty = Success mempty
  {-# INLINE mempty #-}

instance Foldable (Validation err) where
  foldr foldResult accum (Success result) = foldResult result accum
  foldr _ accum (Failure _err) = accum
  {-# INLINE foldr #-}

instance Traversable (Validation err) where
  traverse traverseResult (Success result) = Success <$> traverseResult result
  traverse _ (Failure err) = pure $ Failure err
  {-# INLINE traverse #-}

-------------------------------------------------------------------------------
-- 'bifunctors' typeclass instances

#ifdef SEMIGROUPOIDS

instance Bifunctor Validation where
  bimap mapErr _ (Failure err) = Failure $ mapErr err
  bimap _ mapResult (Success result) = Success $ mapResult result
  {-# INLINE bimap #-}

instance Bifoldable Validation where
  bifoldr foldErr _ accum (Failure err) =
    foldErr err accum
  bifoldr _ foldResult accum (Success result) =
    foldResult result accum
  {-# INLINE bifoldr #-}

instance Bitraversable Validation where
  bitraverse traverseErr _ (Failure err) =
    Failure <$> traverseErr err
  bitraverse _ traverseResult (Success result) =
    Success <$> traverseResult result
  {-# INLINE bitraverse #-}

#endif

-------------------------------------------------------------------------------
-- 'semigroupoids' typeclass instances

#ifdef SEMIGROUPOIDS

instance Semigroup err => Apply (Validation err) where
  (<.>) = apValidation
  {-# INLINE (<.>) #-}

instance Alt (Validation err) where
  (<!>) = altValidation
  {-# INLINE (<!>) #-}

#endif

-------------------------------------------------------------------------------
-- Utility functions

-- | TODO(jkachmar): Documentation.
runValidation :: (err -> out) -> (result -> out) -> Validation err result -> out
runValidation handleErr _ (Failure err) = handleErr err
runValidation _ handleResult (Success result) = handleResult result

-- | TODO(jkachmar): Documentation.
andThen :: Validation err a -> (a -> Validation err b) -> Validation err b
andThen v f = case v of
  (Failure err) -> Failure err
  Success result -> f result
{-# INLINE andThen #-}

-------------------------------------------------------------------------------
-- Typeclass instance implementation functions.

-- | TODO(jkachmar): Documentation.
apValidation ::
  Semigroup err =>
  Validation err (a -> b) ->
  Validation err a ->
  Validation err b
Failure err0 `apValidation` v = Failure $ case v of
  Failure err1 -> err0 <> err1
  Success _result -> err0
Success _result `apValidation` Failure err =
  Failure err
Success f `apValidation` Success a =
  Success (f a)
{-# INLINE apValidation #-}

-- | TODO(jkachmar): Documentation.
altValidation ::
  Validation err result ->
  Validation err result ->
  Validation err result
Failure _err `altValidation` v = v
result@(Success _) `altValidation` _v = result
{-# INLINE altValidation #-}
