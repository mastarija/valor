{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--
module Data.Valor where
--
import Control.Applicative (Alternative (..))
--

newtype Valid v = Valid
  { unValid :: v
  }

instance Semigroup v => Semigroup (Valid v) where
  Valid b <> Valid d = Valid $ b <> d

instance Monoid a => Monoid (Valid a) where
  mempty = Valid mempty

instance Functor Valid where
  fmap f (Valid v) = Valid $ f v

instance Applicative Valid where
  pure = Valid
  Valid f <*> Valid v = Valid $ f v

--

data Wrong e
  = Empty'
  | Amiss'
  | Inert' e
  | Wrong' e

instance Semigroup e => Semigroup (Wrong e) where
  Empty'   <> r        = r
  r        <> Empty'   = r

  Amiss'   <> Amiss'   = Amiss'
  Amiss'   <> Inert' e = Wrong' e
  Amiss'   <> Wrong' e = Wrong' e

  Inert' e <> Amiss'   = Wrong' e
  Inert' b <> Inert' d = Inert' $ b <> d
  Inert' b <> Wrong' d = Wrong' $ b <> d

  Wrong' e <> Amiss'   = Wrong' e
  Wrong' b <> Inert' d = Wrong' $ b <> d
  Wrong' b <> Wrong' d = Wrong' $ b <> d

instance Monoid e => Monoid (Wrong e) where
  mempty = Inert' mempty

instance Functor Wrong where
  fmap _ Empty'     = Empty'
  fmap f (Inert' e) = Inert' $ f e
  fmap f (Wrong' e) = Wrong' $ f e

instance Applicative Wrong where
  pure = Inert'

  Empty'   <*> Amiss'   = Amiss'
  Empty'   <*> Wrong' _ = Amiss'
  Empty'   <*> _        = Empty'

  Amiss'   <*> _        = Amiss'
  _        <*> Amiss'   = Amiss'

  Inert' f <*> Inert' e = Inert' $ f e
  Inert' f <*> Wrong' e = Wrong' $ f e

  Wrong' f <*> Inert' e = Wrong' $ f e
  Wrong' f <*> Wrong' e = Wrong' $ f e

instance Alternative Wrong where
  empty = Amiss'

  Empty'   <|> Inert' e   = Inert' e
  Empty'   <|> _          = Empty'

  Amiss'   <|> r          = r

  Inert' e <|> _          = Inert' e

  Wrong' e <|> Empty'     = Empty'
  Wrong' e <|> Amiss'     = Wrong' e
  Wrong' _ <|> Inert' e   = Inert' e
  Wrong' _ <|> Wrong' e   = Wrong' e

--

newtype Validator m v e = Validator
  { runValidator :: v -> m (Wrong e)
  }

instance (Applicative m, Semigroup e) => Semigroup (Validator m v e) where
  Validator b <> Validator d = Validator $ \v -> (<>) <$> (b v) <*> (d v)

instance (Applicative m, Monoid e) => Monoid (Validator m v e) where
  mempty = Validator $ const $ pure mempty

instance Functor m => Functor (Validator m v) where
  fmap f (Validator val) = Validator $ \v -> (fmap f) <$> (val v)

instance Applicative m => Applicative (Validator m v) where
  pure = Validator . const . pure . pure
  Validator b <*> Validator d = Validator $ \v -> (<*>) <$> (b v) <*> (d v)

instance Applicative m => Alternative (Validator m v) where
  empty = Validator $ const $ pure Empty'
  Validator b <|> Validator d = Validator $ \v -> (<|>) <$> (b v) <*> (d v)

instance Monad m => Monad (Validator m v) where
  Validator ve >>= e_vn = Validator $ \v -> do
    re <- ve v
    case re of
      Empty'   -> pure Empty'
      Amiss'   -> pure Amiss'
      Inert' e -> runValidator (e_vn e) v
      Wrong' e -> runValidator (e_vn e) v

--

passV :: (Applicative m, Monoid e) => Validator m v e
passV = Validator $ const $ pure $ mempty

failV :: Applicative m => e -> Validator m v e
failV = Validator . const . pure . Wrong'

turnM :: (Functor m, Monoid e) => Validator m v e -> Validator m v (Maybe e)
turnM (Validator val) = Validator $ \v -> flip fmap (val v) $ \re -> case re of
  Empty'   -> Inert' $ Nothing
  Amiss'   -> Wrong' $ Just mempty
  Inert' e -> Inert' $ Nothing
  Wrong' e -> Inert' $ Just e

turnE :: (Functor m, Monoid e) => Validator m v e -> Validator m v (Either (Valid v) e)
turnE (Validator val) = Validator $ \v -> flip fmap (val v) $ \re -> case re of
  Empty'   -> Inert' $ Left $ Valid v
  Amiss'   -> Wrong' $ Right mempty
  Inert' e -> Inert' $ Left $ Valid v
  Wrong' e -> Inert' $ Right e

--

class Check f v o where
  check :: f -> v -> o

instance (Functor m, Monoid e) => Check (v -> x) (Validator m x e) (Validator m v (Maybe e)) where
  check sel val = turnM $ Validator $ \v -> runValidator val $ sel v

instance (Applicative m, Monoid e) => Check (v -> x) [Validator m x e] (Validator m v (Maybe e)) where
  check sel vals = check sel $ mconcat vals

instance (Traversable t, Applicative m, Monoid e, Monoid (t (Maybe e))) => Check (v -> t x) (Validator m x e) (Validator m v (Maybe (t (Maybe e)))) where
  check sel val = turnM $ Validator $ \v -> sequenceA <$> traverse (runValidator $ turnM val) (sel v)

instance (Traversable t, Applicative m, Monoid e, Monoid (t (Maybe e))) => Check (v -> t x) [Validator m x e] (Validator m v (Maybe (t (Maybe e)))) where
  check sel vals = check sel $ mconcat vals
