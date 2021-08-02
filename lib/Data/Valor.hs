{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--
module Data.Valor
  ( Valid
  , Validator
  , check
  , passV
  , failV
  , poolV
  , boolV
  , turnM
  , turnE
  )
  where
--
import Data.Bool (bool)
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
  = Inert e
  | Wrong e

instance Semigroup e => Semigroup (Wrong e) where
  Inert b <> Inert d = Inert $ b <> d
  Inert b <> Wrong d = Wrong $ b <> d
  Wrong b <> Inert d = Wrong $ b <> d
  Wrong b <> Wrong d = Wrong $ b <> d

instance Monoid e => Monoid (Wrong e) where
  mempty = Inert mempty

instance Functor Wrong where
  fmap f (Inert e) = Inert $ f e
  fmap f (Wrong e) = Wrong $ f e

instance Applicative Wrong where
  pure = Inert

  Inert f <*> Inert e = Inert $ f e
  Inert f <*> Wrong e = Wrong $ f e
  Wrong f <*> Inert e = Wrong $ f e
  Wrong f <*> Wrong e = Wrong $ f e

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

instance Monad m => Monad (Validator m v) where
  Validator val >>= e_wc = Validator $ \v -> val v >>= \we -> case we of
    Inert e -> runValidator (e_wc e) v
    Wrong e -> runValidator (e_wc e) v

--

pickV :: Monad m => Validator m v e -> Validator m v e -> Validator m v e
(Validator b) `pickV` (Validator d) = Validator $ \v -> do
  wb <- b v
  wd <- d v
  pure $ case (wb, wd) of
    (Inert e, _) -> Inert e
    (Wrong _, w) -> w

pickV' :: (Monad m, Semigroup e) => Validator m v e -> Validator m v e -> Validator m v e
(Validator b) `pickV'` (Validator d) = Validator $ \v -> do
  wb <- b v
  wd <- d v
  pure $ case (wb, wd) of
    (Inert b, Inert d) -> Inert $ b <> d
    (Inert b, Wrong _) -> Inert b
    (Wrong b, Inert d) -> Inert d
    (Wrong b, Wrong d) -> Wrong $ b <> d

caseV :: Monad m => Validator m v e -> Validator m v e -> Validator m v e -> Validator m v e
caseV (Validator p) (Validator b) (Validator d) = Validator $ \v -> do
  wp <- p v
  case wp of
    Inert _ -> (d v)
    Wrong _ -> (b v)

caseV' :: (Monad m, Semigroup e) => Validator m v e -> Validator m v e -> Validator m v e -> Validator m v e
caseV' (Validator p) (Validator b) (Validator d) = Validator $ \v -> do
  wp <- p v
  case wp of
    Inert _ -> (<>) <$> (pure wp) <*> (d v)
    Wrong _ -> (<>) <$> (pure wp) <*> (b v)

--

passV :: (Applicative m, Monoid e) => Validator m v e
passV = mempty

failV :: Applicative m => e -> Validator m v e
failV = Validator . const . pure . Wrong

--

poolV :: Monad m => (v -> Bool ) -> Validator m v e -> Validator m v e -> Validator m v e
poolV p = boolV $ pure . p

boolV :: Monad m => (v -> m Bool) -> Validator m v e -> Validator m v e -> Validator m v e
boolV p b d = Validator $ \v -> p v >>= flip runValidator v . bool b d

--

turnM :: (Functor m, Monoid e) => Validator m v e -> Validator m v (Maybe e)
turnM (Validator val) = Validator $ \v -> flip fmap (val v) $ \re -> case re of
  Inert e -> Inert $ Nothing
  Wrong e -> Wrong $ Just e

turnE :: (Functor m, Monoid e) => Validator m v e -> Validator m v (Either (Valid v) e)
turnE (Validator val) = Validator $ \v -> flip fmap (val v) $ \re -> case re of
  Inert e -> Inert $ Left $ Valid v
  Wrong e -> Wrong $ Right e

--

class Check f v o where
  check :: f -> v -> o

instance (Functor m, Monoid e) =>
  Check (v -> x) (Validator m x e) (Validator m v (Maybe e)) where
  check sel val = turnM $ Validator $ \v -> runValidator val $ sel v

instance (Applicative m, Monoid e) =>
  Check (v -> x) [Validator m x e] (Validator m v (Maybe e)) where
  check sel vals = check sel $ mconcat vals

instance (Traversable t, Applicative m, Monoid e, Monoid (t (Maybe e))) =>
  Check (v -> t x) (Validator m x e) (Validator m v (Maybe (t (Maybe e)))) where
  check sel val = turnM $ Validator $ \v -> sequenceA <$> traverse (runValidator $ turnM val) (sel v)

instance (Traversable t, Applicative m, Monoid e, Monoid (t (Maybe e))) =>
  Check (v -> t x) [Validator m x e] (Validator m v (Maybe (t (Maybe e)))) where
  check sel vals = check sel $ mconcat vals

instance forall m v e c x. (Applicative m, Applicative c, Monoid (c e) )
  => Check (v -> x) [Validator m x e] (Validator m v (Maybe (c e))) where
  check sel vals = check sel $ (((fmap pure) <$> vals) :: [Validator m x (c e)])

instance forall m v e t c x. (Applicative m, Applicative c, Traversable t, Monoid (c e), (Monoid (t (Maybe (c e))))) =>
  Check (v -> t x) [Validator m x e] (Validator m v (Maybe (t (Maybe (c e))))) where
  check sel vals = check sel $ (((fmap pure) <$> vals) :: [Validator m x (c e)])

--

data Cred = Cred
  { username :: UserName
  , password :: PassWord
  }

newtype UserName = UserName
  { unUserName :: String
  }

newtype PassWord = PassWord
  { unPassWord :: String
  }

usernameV :: Validator m UserName String
usernameV = undefined