{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--
module Data.Valor where
--
import Data.Maybe (isNothing)
import Data.Functor.Identity (Identity (..))
--
import Data.Valor.Internal
--

newtype Valid v = Valid
  { unValid :: v
  } deriving (Eq, Show)

--

newtype Validator m v e = Validator
  { runValidator :: v -> m (Wrong e)
  }

instance (Monad m, Semigroup e) => Semigroup (Validator m v e) where
  Validator b <> Validator d = Validator $ \v -> (<>) <$> (b v) <*> (d v)

instance (Monad m, Monoid e) => Monoid (Validator m v e) where
  mempty = Validator $ const $ pure mempty

instance Monad m => Functor (Validator m v) where
  fmap f (Validator val) = Validator $ \v -> (fmap f) <$> (val v)

instance Monad m => Applicative (Validator m v) where
  pure = Validator . const . pure . pure
  Validator b <*> Validator d = Validator $ \v -> (<*>) <$> (b v) <*> (d v)

--

altOV :: Monad m => Validator m v e -> Validator m v e -> Validator m v e
altOV (Validator b) (Validator d) = Validator $ \v -> altOW <$> (b v) <*> (d v)

altMV :: (Monad m, Semigroup e) => Validator m v e -> Validator m v e -> Validator m v e
altMV (Validator b) (Validator d) = Validator $ \v -> altMW <$> (b v) <*> (d v)

--

passV :: (Monad m, Monoid e) => Validator m v e
passV = mempty

failV :: Monad m => e -> Validator m v e
failV = Validator . const . pure . Wrong

makeV :: (Monad m, Monoid e) => (v -> m (Maybe e)) -> Validator m v e
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
  case vp of
    Inert _  -> d v
    Wrong e1 -> do
      vb <- b v
      case vb of
        Inert _ -> pure vb
        Wrong e2 -> pure $ Wrong $ e1 <> e2

--

class Check v x i e o where
  check :: Monad m => (v -> x) -> Validator m i e -> Validator m v o

instance Check v x x e (Maybe e) where
  check sel val = Validator $ \v -> runValidator (go val) (sel v)
    where
      go (Validator vl) = Validator $ \v -> flip fmap (vl v) $ \we -> case we of
        Inert _ -> Inert $ Nothing
        Wrong e -> Wrong $ Just e

instance Traversable t => Check v (t x) x e (Maybe (t (Maybe e))) where
  check sel (Validator val) = Validator $ fmap (go2 . go1) . traverse val . sel
    where
      go1 ws = flip fmap ws $ \w -> case w of
        Inert _ -> Nothing
        Wrong e -> Just e

      go2 ms
        | all isNothing ms = Inert Nothing
        | otherwise        = Wrong $ Just ms

oneCheck :: Monad m => (v -> x) -> Validator m x e -> Validator m v (Maybe e)
oneCheck = check

oneChecks :: (Monad m, Monoid e) => (v -> x) -> [Validator m x e] -> Validator m v (Maybe e)
oneChecks sel vals = check sel $ mconcat vals

mapCheck :: (Monad m, Traversable t) => (v -> t x) -> Validator m x e -> Validator m v (Maybe (t (Maybe e)))
mapCheck = check

mapChecks :: (Monad m, Traversable t, Monoid e) => (v -> t x) -> [Validator m x e] -> Validator m v (Maybe (t (Maybe e)))
mapChecks sel = check sel . mconcat

--

validateM :: Functor m => Validator m v e -> v -> m (Either (Valid v) e)
validateM val v = flip fmap (runValidator val v) $ \w -> case w of
  Inert _ -> Left $ Valid v
  Wrong e -> Right e

validateP :: Validator Identity v e -> v -> Either (Valid v) e
validateP val = runIdentity . validateM val

