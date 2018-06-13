{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
--
module Valor where
--
import Valor.Utils
import Data.Text ( Text )
import GHC.Generics
import Data.Functor.Identity
import Data.List.NonEmpty ( NonEmpty (..), toList )
import Data.Semigroup
import Control.Monad.Trans.Except
--

data Validated e = Valid e | Invalid e
  deriving ( Eq, Show, Generic )

instance Semigroup e => Semigroup (Validated e) where
  Valid _    <> x          = x
  x          <> Valid _    = x
  Invalid e1 <> Invalid e2 = Invalid (e1 <> e2)

instance (Semigroup e, Monoid e) => Monoid (Validated e) where
  mempty  = Valid mempty
  mappend = (<>)

instance Functor Validated where
  fmap f (Valid   e) = Valid   (f e)
  fmap f (Invalid e) = Invalid (f e)

instance Applicative Validated where
  pure                     = Valid
  Valid   ef <*> Valid   e = Valid   $ ef e
  Valid   ef <*> Invalid e = Invalid $ ef e
  Invalid ef <*> Valid   e = Invalid $ ef e
  Invalid ef <*> Invalid e = Invalid $ ef e

--

newtype Validator i m e = Validator
  { unValidator :: i -> m (Validated e)
  }

instance Semigroup (m (Validated e)) => Semigroup (Validator i m e) where
  Validator x <> Validator y = Validator $ \i -> x i <> y i

instance (Semigroup (m (Validated e)), Semigroup e, Monoid e, Applicative m) => Monoid (Validator i m e) where
  mempty = Validator $ \i -> pure mempty
  mappend = (<>)

instance Functor m => Functor (Validator i m) where
  fmap f (Validator v) = Validator $ fmap (fmap f) . v

instance Applicative m => Applicative (Validator i m) where
  pure x = Validator $ \i -> pure $ pure x
  Validator x <*> Validator y = Validator $ \i ->
    (<*>) <$> x i <*> y i

--

data User = User
  { username :: String
  , password :: String
  } deriving ( Show )

data UserError = UserError
  { username :: Maybe [String]
  , password :: Maybe [String]
  } deriving ( Show )

validateUser :: User -> Validated UserError
validateUser user = UserError
  <$> field ((nonempty $ r @User username user) <> (nonshit $ r @User username user))
  <*> field ((nonempty $ r @User password user) <> (nonshit $ r @User password user))

field :: Validated e -> Validated (Maybe e)
field (Valid _)   = Valid Nothing
field (Invalid e) = Invalid $ Just e

nonempty :: String -> Validated [String]
nonempty s
  | length s == 0 = Invalid ["can't be empty"]
  | otherwise     = mempty

nonshit :: String -> Validated [String]
nonshit "shit" = Invalid ["can't be shit"]
nonshit _      = mempty

user :: User
user = User "" "lele"

--

validateUser' :: Monad m => Validator User m UserError
validateUser' = UserError
  <$> (field' @User username nonempty')
  <*> (field' @User password nonempty')

nonempty' :: Monad m => String -> ExceptT [String] m String
nonempty' s
  | length s == 0 = throwE ["can't be empty"]
  | otherwise = pure s

converter
  :: forall i m e e'. Monad m
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

m :: Monad m => Validator i m e -> Validator i m (Maybe e)
m (Validator v) = undefined

field'
  :: forall i e m x. (Semigroup e, Monoid e, Monad m)
  => (i -> x)
  -> (x -> ExceptT e m x)
  -> Validator i m (Maybe e)
field' selector = converter (const Nothing) Just . validation' selector

validation'
  :: forall i e m x. (Semigroup e, Monoid e, Monad m)
  => (i -> x)
  -> (x -> ExceptT e m x)
  -> Validator i m e
validation' selector checker = Validator $ \i -> do
  validated <- runExceptT $ checker $ selector i
  pure $ case validated of
    Left e  -> Invalid e
    Right _ -> mempty
