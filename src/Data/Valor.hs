-- |
-- Module      :  Valor
-- Copyright   :  © 2018 Luka Hadžiegrić
-- License     :  MIT
--
-- Maintainer  :  Luka Hadžiegrić <reygoch@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
--
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
module Data.Valor
  ( -- * Constructing a 'Validator'
    Validator
  , skip
  , check
  , checks
  , mapcheck
  , mapchecks
  , enterField
    -- * Validating data
  , validate
  , validatePure
  ) where
--
import Data.Maybe ( isJust )
import Data.Semigroup ( Semigroup, (<>) )

import Control.Applicative ( liftA2 )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )

import Data.Functor.Identity ( Identity (..) )
--

--------------------------------------------------------------------------------
-- | Internal datatype used for handling users error structure.
data Validated e = Valid e | Invalid e
  deriving ( Show )

instance Semigroup e => Semigroup (Validated e) where
  Valid _    <> x          = x
  x          <> Valid _    = x
  Invalid e1 <> Invalid e2 = Invalid (e1 <> e2)

instance Functor Validated where
  fmap f (Valid   e) = Valid   $ f e
  fmap f (Invalid e) = Invalid $ f e

instance Applicative Validated where
  pure                     = Valid
  Valid   ef <*> Valid   e = Valid   $ ef e
  Valid   ef <*> Invalid e = Invalid $ ef e
  Invalid ef <*> Valid   e = Invalid $ ef e
  Invalid ef <*> Invalid e = Invalid $ ef e


--------------------------------------------------------------------------------
-- | This type represents a 'Validator' that can be run on your custom data
-- types with 'validate' or 'validatePure'. You can use custom monad for your
-- validations and you are free to define your value and error types by hand, or
-- by using provided 'Validatable' type family.
--
-- 'Validator' is constructed by using 'skip', 'check', 'mapcheck', 'checks',
-- 'mapchecks' and 'enterField'.
newtype Validator i m e = Validator
  { unValidator :: i -> m (Validated e)
  }

instance (Applicative m, Semigroup e) => Semigroup (Validator i m e) where
  Validator x <> Validator y = Validator $ \i -> liftA2 (<>) (x i) (y i)

instance Functor m => Functor (Validator i m) where
  fmap f (Validator v) = Validator $ fmap (fmap f) . v

instance Applicative m => Applicative (Validator i m) where
  pure x = Validator $ \i -> pure $ pure x
  Validator x <*> Validator y = Validator $ \i ->
    (<*>) <$> x i <*> y i


--------------------------------------------------------------------------------
-- | Once you have constructed your 'Validator' you can run it against your
-- input data. If there were no validation errory you will get 'Nothing' wrapped
-- in a monad of your choice as a result.
validate :: (Functor m) => Validator i m e -> i -> m (Maybe e)
validate (Validator v) i = helper <$> v i
  where
    helper :: Validated e -> Maybe e
    helper (Invalid e) = Just e
    helper _           = Nothing


--------------------------------------------------------------------------------
-- | This will run your 'Validator' as a pure computation returning simple
-- 'Maybe' instead of it being wrapped in some monad.
validatePure :: Validator i Identity e -> i -> Maybe e
validatePure v i = runIdentity $ validate v i


--------------------------------------------------------------------------------
-- Utility function used for converting 'Validator' error from one type to
-- another (it's not very useful).
converter
  :: forall i m e e'. Functor m
  => (e -> e')
  -> (e -> e')
  -> Validator i m e
  -> Validator i m e'
converter valid invalid (Validator v) = Validator $ \i ->
  fmap helper $ v i
  where
    helper :: Validated e -> Validated e'
    helper (Valid   e) = Valid   $ valid   e
    helper (Invalid e) = Invalid $ invalid e


--------------------------------------------------------------------------------
-- | Another simple utility function for converting 'Validator' error this time
-- in to a 'Maybe'.
mconvert :: Functor m => Validator i m e -> Validator i m (Maybe e)
mconvert = converter (const Nothing) Just


--------------------------------------------------------------------------------
-- | Internal utility function for constructing a 'Validator' from 'ExceptT'.
validator :: (Functor m, Monoid e)
  => (i -> ExceptT e m x)
  -> Validator i m e
validator chk = Validator $ \i ->
  either Invalid (const $ Valid mempty) <$> (runExceptT $ chk i)


--------------------------------------------------------------------------------
-- | This will allow you to not validate a certain field.
skip :: Applicative m => Validator i m (Maybe e)
skip = Validator $ \_ -> pure $ Valid Nothing

--

check :: forall i x m e. (Functor m, Monoid e)
  => (i -> x)
  -> (x -> ExceptT e m x)
  -> Validator i m (Maybe e)
check sel chk = mconvert $ Validator $ unValidator (validator chk) . sel

--

mapcheck :: forall i f x m e. (Traversable f, Foldable f, Monad m, Monoid e)
  => (i -> f x)
  -> (x -> ExceptT e m x)
  -> Validator i m (Maybe (f (Maybe e)))
mapcheck sel chk = Validator $ \i -> do
  res <- mapM (validate $ validator chk) (sel i)
  pure $ if all isJust res then Invalid $ Just res else Valid Nothing

--

checks
  :: forall i x m e. ( Applicative m, Monoid e, Semigroup e )
  => (i -> x)
  -> [x -> ExceptT e m x]
  -> Validator i m (Maybe e)
checks sel chks = foldr1 (<>) $ fmap (check sel) chks

--

mapchecks
  :: forall i f x m e.
  ( Monad m
  , Monoid e
  , Traversable f
  , Semigroup (f (Maybe e))
  )
  => (i -> f x)
  -> [x -> ExceptT e m x]
  -> Validator i m (Maybe (f (Maybe e)))
mapchecks sel chks = foldr1 (<>) $ fmap (mapcheck sel) chks

--

enterField :: forall i x m e. (Functor m)
  => (i -> x)
  -> Validator x m e
  -> Validator i m (Maybe e)
enterField sel (Validator x) = mconvert $ Validator $ \i -> x $ sel i

--------------------------------------------------------------------------------

data Validate a

type family Validatable a e x where
  Validatable Validate e x = Maybe e
  Validatable Identity e x = x
  Validatable a        e x = a x
