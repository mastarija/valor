{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
--
module Valor where
--
import Data.Semigroup
import Control.Applicative ( liftA2 )
import Control.Monad.Trans.Except
--

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

validator :: (Functor m, Monoid e)
  => (i -> ExceptT e m i)
  -> Validator i m e
validator chk = Validator $ \i ->
  either Invalid (const $ Valid mempty) <$> (runExceptT $ chk i)

--

fieldCheck :: forall i x m e. (Functor m, Monoid e)
  => (i -> x)
  -> (x -> ExceptT e m x)
  -> Validator i m (Maybe e)
fieldCheck sel chk = mconvert $ Validator $ unValidator (validator chk) . sel

--

fieldChecks :: forall i x m e. ( Applicative m, Monoid e, Semigroup e )
  => (i -> x)
  -> [x -> ExceptT e m x]
  -> Validator i m (Maybe e)
fieldChecks sel chks = foldr1 (<>) $ fmap (fieldCheck sel) chks

--

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

enterField :: forall i x m e. (Functor m)
  => (i -> x)
  -> Validator x m e
  -> Validator i m (Maybe e)
enterField sel (Validator x) = mconvert $ Validator $ \i -> x $ sel i

--------------------------------------------------------------------------------

data Form = Form
  { age  :: Int
  , user :: User
  } deriving ( Show )

data FormError = FormError
  { age :: Maybe [String]
  , user :: Maybe UserError
  } deriving ( Show )

data User = User
  { username :: String
  , password :: String
  } deriving ( Show )

data UserError = UserError
  { username :: Maybe [String]
  , password :: Maybe [String]
  } deriving ( Show )

badUser :: User
badUser = User "shit" ""

goodUser :: User
goodUser = User "squeaky" "clean"

badForm :: Form
badForm = Form 12 badUser

partiallyBadForm :: Form
partiallyBadForm = Form 12 goodUser

formValidator :: Monad m => Validator Form m FormError
formValidator = FormError
  <$> fieldCheck @Form age over18
  <*> enterField @Form user userValidator

userValidator :: Monad m => Validator User m UserError
userValidator = UserError
  <$> fieldChecks @User username [nonempty, nonshit, nonshort]
  <*> fieldChecks @User password [nonempty]

over18 :: Monad m => Int -> ExceptT [String] m Int
over18 age
  | age < 18 = throwE ["fuck off kiddo"]
  | otherwise = pure age

nonempty :: Monad m => String -> ExceptT [String] m String
nonempty s
  | length s == 0 = throwE ["can't be empty"]
  | otherwise     = pure s

nonshit :: Monad m => String -> ExceptT [String] m String
nonshit s
  | s == "shit" = throwE ["can't be shit"]
  | otherwise   = pure s

nonshort :: Monad m => String -> ExceptT [String] m String
nonshort s
  | length s <= 4 = throwE ["must have min 5 letters"]
  | otherwise     = pure s