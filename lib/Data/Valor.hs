{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
--
module Data.Valor where
--
import Data.Maybe ( isJust )
import Data.Functor.Identity ( Identity , runIdentity )
import Control.Applicative ( Alternative (..) , liftA2 )
import Control.Monad.Trans.Except ( ExceptT , runExceptT )
--

data Normal
data Report

type family Validatable a e x where
  Validatable Normal e x = x
  Validatable Report e x = Maybe e

--

data Status e = Neutral | Inert e | Wrong e

instance Semigroup e => Semigroup (Status e) where
  Neutral <> x       = x
  x       <> Neutral = x
  Inert _ <> x       = x
  x       <> Inert _ = x
  Wrong a <> Wrong b = Wrong $ a <> b

instance Semigroup e => Monoid (Status e) where
  mempty = Neutral
  mappend = (<>)

instance Functor Status where
  fmap _ Neutral   = Neutral
  fmap f (Inert e) = Inert $ f e
  fmap f (Wrong e) = Wrong $ f e

instance Applicative Status where
  pure                    = Inert
  (Inert f) <*> (Inert e) = Inert $ f e
  (Inert f) <*> (Wrong e) = Wrong $ f e
  (Wrong f) <*> (Inert e) = Wrong $ f e
  (Wrong f) <*> (Wrong e) = Wrong $ f e

instance Alternative Status where
  empty = Neutral

  Neutral  <|> x        = Neutral
  x        <|> Neutral  = Neutral
  Inert e  <|> x        = Inert e
  x        <|> Inert e  = Inert e
  Wrong _  <|> Wrong e  = Wrong e

--

newtype Validator i m e = Validator
  { unValidator :: i -> m (Status e)
  }

instance (Applicative m, Semigroup e) => Semigroup (Validator i m e) where
  Validator x <> Validator y = Validator $ \i -> liftA2 (<>) (x i) (y i)

instance ( Applicative m , Semigroup e ) => Monoid (Validator i m e) where
  mempty = Validator $ const $ pure Neutral
  mappend = (<>)

instance Functor m => Functor (Validator i m) where
  fmap f (Validator v) = Validator $ \i -> fmap (fmap f) (v i)

instance Applicative m => Applicative (Validator i m) where
  pure = Validator . const . pure . pure
  Validator x <*> Validator y = Validator $ \i -> (<*>) <$> x i <*> y i

instance Applicative m => Alternative (Validator i m) where
  empty = Validator $ const $ pure Neutral
  (Validator f1) <|> (Validator f2) = Validator $ \i -> liftA2 (<|>) (f1 i) (f2 i)

instance Monad m => Monad (Validator i m) where
  ma >>= a_mb = Validator $ \i -> unValidator ma i >>= \case
    Neutral -> pure Neutral
    Inert a -> unValidator ( a_mb a ) i
    Wrong a -> unValidator ( a_mb a ) i

--

pass :: Applicative m => Validator i m e
pass = Validator $ const $ pure Neutral

fail :: Applicative m => e -> Validator i m e
fail = Validator . const . pure . Wrong

--

newtype Valid a = Valid
  { unValid :: a
  }

-- check :: Monad m => ( i -> x ) -> ( x -> ExceptT e m x ) -> Validator i m e
-- check sel chk = Validator $ \i -> runExceptT ( chk $ sel i )
