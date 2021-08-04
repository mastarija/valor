{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--
module Data.Valor where
--
import Data.Maybe (isJust)
import Data.Functor.Identity (Identity (..))
--
import Data.Valor.Internal
--

newtype Valid v = Valid
  { unValid :: v
  }

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

--

altOV :: Applicative m => Validator m v e -> Validator m v e -> Validator m v e
altOV (Validator b) (Validator d) = Validator $ \v -> altOW <$> (b v) <*> (d v)

altMV :: (Applicative m, Semigroup e) => Validator m v e -> Validator m v e -> Validator m v e
altMV (Validator b) (Validator d) = Validator $ \v -> altMW <$> (b v) <*> (d v)

--

passV :: (Applicative m, Monoid e) => Validator m v e
passV = mempty

failV :: Applicative m => e -> Validator m v e
failV = Validator . const . pure . Wrong

makeV :: (Functor m, Monoid e) => (v -> m (Maybe e)) -> Validator m v e
makeV c = Validator $ \v -> flip fmap (c v) $ \me -> case me of
  Nothing -> Inert mempty
  Just e  -> Wrong e

--

testV :: Monad m => Validator m v e -> Validator m v e -> (v -> m Bool) -> Validator m v e
testV (Validator b) (Validator d) p = Validator $ \v -> p v >>= \c -> if not c then (b v) else (d v)

caseV :: Monad m => Validator m v e -> Validator m v e -> Validator m v e -> Validator m v e
caseV (Validator b) (Validator d) (Validator p) = Validator $ \v -> do
  vp <- p v
  case vp of
    Inert _ -> d v
    Wrong _ -> b v

scanV :: (Monad m, Monoid e) => Validator m v e -> Validator m v e -> Validator m v e -> Validator m v e
scanV (Validator b) (Validator d) (Validator p) = Validator $ \v -> do
  vp <- p v
  (<>) <$> (pure vp) <*> case vp of
    Inert _ -> d v
    Wrong _ -> b v

--

class Check f v o where
  check :: f -> v -> o

instance Functor m =>
  Check (v -> x) (Validator m x e) (Validator m v (Maybe e)) where
  check sel val = Validator $ \v -> runValidator (go val) (sel v)
    where
    go :: Functor m => Validator m v e -> Validator m v (Maybe e)
    go (Validator vl) = Validator $ \v -> flip fmap (vl v) $ \we -> case we of
      Inert _ -> Inert $ Nothing
      Wrong e -> Wrong $ Just e

instance (Applicative m, Monoid e) =>
  Check (v -> x) [Validator m x e] (Validator m v (Maybe e)) where
  check sel = check sel . mconcat

instance (Applicative m, Traversable t) =>
  Check (v -> t x) (Validator m x e) (Validator m v (Maybe (t (Maybe e)))) where
  check sel (Validator val) = Validator $ fmap (go2 . go1) . traverse val . sel
    where
      go1 ws = flip fmap ws $ \w -> case w of
        Inert _ -> Nothing
        Wrong e -> Just e

      go2 ms
        | all isJust ms = Inert Nothing
        | otherwise     = Wrong $ Just ms

instance (Applicative m, Traversable t, Monoid e) =>
  Check (v -> t x) [Validator m x e] (Validator m v (Maybe (t (Maybe e)))) where
  check sel = check sel . mconcat

--

validateM :: Functor m => Validator m v e -> v -> m (Either (Valid v) e)
validateM val v = flip fmap (runValidator val v) $ \w -> case w of
  Inert _ -> Left $ Valid v
  Wrong e -> Right e

validateP :: Validator Identity v e -> v -> Either (Valid v) e
validateP val = runIdentity . validateM val
