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

data Status e = Inert e | Wrong e

instance Semigroup e => Semigroup (Status e) where
  Inert _ <> x       = x
  x       <> Inert _ = x
  Wrong a <> Wrong b = Wrong $ a <> b

instance Monoid e => Monoid (Status e) where
  mempty = Inert mempty
  mappend = (<>)

instance Functor Status where
  fmap f (Inert e) = Inert $ f e
  fmap f (Wrong e) = Wrong $ f e

instance Applicative Status where
  pure                    = Inert
  (Inert f) <*> (Inert e) = Inert $ f e
  (Inert f) <*> (Wrong e) = Wrong $ f e
  (Wrong f) <*> (Inert e) = Wrong $ f e
  (Wrong f) <*> (Wrong e) = Wrong $ f e

instance Monad Status where
  Inert a >>= a_mb = a_mb a
  Wrong a >>= a_mb = a_mb a

--

newtype Validator i m e = Validator
  { unValidator :: i -> m (Status e)
  }

instance (Applicative m, Semigroup e) => Semigroup (Validator i m e) where
  Validator x <> Validator y = Validator $ \i -> liftA2 (<>) (x i) (y i)

instance ( Applicative m , Monoid e ) => Monoid (Validator i m e) where
  mempty = Validator $ const $ pure mempty
  mappend = (<>)

instance Functor m => Functor (Validator i m) where
  fmap f (Validator v) = Validator $ \i -> fmap (fmap f) (v i)

instance Applicative m => Applicative (Validator i m) where
  pure = Validator . const . pure . pure
  Validator x <*> Validator y = Validator $ \i -> (<*>) <$> x i <*> y i

instance Monad m => Monad (Validator i m) where
  Validator v >>= a_mb = Validator $ \i -> v i >>= flip unValidator i . a_mb . stat

--

pass :: (Monoid e, Applicative m) => Validator i m e
pass = Validator $ const $ pure mempty

fail :: Applicative m => e -> Validator i m e
fail = Validator . const . pure . Wrong

--

check :: (i -> x) -> Validator x m e -> Validator i m e
check sel (Validator val) = Validator $ \i -> val $ sel i

mapCheck :: (Traversable f, Applicative m) => (i -> f x) -> Validator x m e -> Validator i m (f e)
mapCheck sel (Validator val) = Validator $ \i -> do
  let es = traverse val $ sel i :: _
  undefined

checks :: (Monoid e, Applicative m) => (i -> x) -> [Validator x m e] -> Validator i m e
checks sel vals = check sel $ mconcat vals


--

stat :: Status e -> e
stat (Inert e) = e
stat (Wrong e) = e

