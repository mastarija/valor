{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
--
module Valor where
--
import Data.Semigroup ( Semigroup, (<>) )
import Control.Applicative ( liftA2 )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )
import Data.Functor.Identity ( Identity (..) )
--

data Validate a

type family Validatable a e x where
  Validatable Validate e x = Maybe e
  Validatable Identity e x = x

--------------------------------------------------------------------------------

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

validate :: (Functor m) => Validator i m e -> i -> m (Maybe e)
validate (Validator v) i = helper <$> v i
  where
    helper :: Validated e -> Maybe e
    helper (Invalid e) = Just e
    helper _           = Nothing

validatePure :: Validator i Identity e -> i -> Maybe e
validatePure v i = runIdentity $ validate v i

--------------------------------------------------------------------------------

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

--

mconvert :: Functor m => Validator i m e -> Validator i m (Maybe e)
mconvert = converter (const Nothing) Just

--

validator :: (Functor m, Monoid e)
  => (i -> ExceptT e m x)
  -> Validator i m e
validator chk = Validator $ \i ->
  either Invalid (const $ Valid mempty) <$> (runExceptT $ chk i)

--
skip :: Applicative m => Validator i m (Maybe e)
skip = Validator $ \_ -> pure $ Valid Nothing

check :: forall i x m e. (Functor m, Monoid e)
  => (i -> x)
  -> (x -> ExceptT e m x)
  -> Validator i m (Maybe e)
check sel chk = mconvert $ Validator $ unValidator (validator chk) . sel

--

checks :: forall i x m e. ( Applicative m, Monoid e, Semigroup e )
  => (i -> x)
  -> [x -> ExceptT e m x]
  -> Validator i m (Maybe e)
checks sel chks = foldr1 (<>) $ fmap (check sel) chks

--

enterField :: forall i x m e. (Functor m)
  => (i -> x)
  -> Validator x m e
  -> Validator i m (Maybe e)
enterField sel (Validator x) = mconvert $ Validator $ \i -> x $ sel i

--------------------------------------------------------------------------------