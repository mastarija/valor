module Data.Valor where
--

newtype Valid v = Valid
  { unValid :: v
  }

--

data Wrong e = Inert e | Wrong e

instance Semigroup e => Semigroup (Wrong e) where
  Wrong b <> Wrong d = Wrong $ b <> d
  Wrong e <> Inert _ = Wrong e
  Inert _ <> Wrong e = Wrong e
  Inert e <> Inert _ = Inert e

instance Monoid e => Monoid (Wrong e) where
  mempty = Inert mempty

instance Functor Wrong where
  fmap f (Wrong e) = Wrong $ f e
  fmap f (Inert e) = Inert $ f e

instance Applicative Wrong where
  pure = Inert

  Wrong f <*> Wrong e = Wrong $ f e
  Wrong f <*> Inert e = Wrong $ f e
  Inert f <*> Wrong e = Wrong $ f e
  Inert f <*> Inert e = Inert $ f e

altOW :: Wrong e -> Wrong e -> Wrong e
altOW (Inert e) _         = Inert e
altOW _         (Inert e) = Inert e
altOW (Wrong _) (Wrong e) = Wrong e

altMW :: Semigroup e => Wrong e -> Wrong e -> Wrong e
altMW (Inert e) _         = Inert e
altMW _         (Inert e) = Inert e
altMW (Wrong b) (Wrong d) = Wrong $ b <> d

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

turnM :: Functor m => Validator m v e -> Validator m v (Maybe e)
turnM (Validator val) = Validator $ \v -> flip fmap (val v) $ \we -> case we of
  Inert _ -> Inert $ Nothing
  Wrong e -> Wrong $ Just e

turnE :: Functor m => Validator m v e -> Validator m v (Either (Valid v) e)
turnE (Validator val) = Validator $ \v -> flip fmap (val v) $ \we -> case we of
  Inert _ -> Inert $ Left (Valid v)
  Wrong e -> Wrong $ Right e

--
