{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
--
module Data.Valor where
--
import Data.Bool (bool)
import Control.Applicative (Alternative (..))
--

newtype FullName = FullName
  { unFullName :: String
  }

newtype UserName = UserName
  { unUserName :: String
  }

newtype PassWord = PassWord
  { unPassWord :: String
  }

data Cred = Cred
  { username :: UserName
  , password :: PassWord
  }

data CredReport = CredReport
  { username :: Maybe [String]
  , password :: Maybe [String]
  }

data User = User
  { username :: UserName
  , fullname :: FullName
  }

data UserReport = UserReport
  { username :: Maybe [String]
  , fullname :: Maybe [String]
  }

data Post = Post
  { name    :: String
  , text    :: String
  , authors :: [ User ]
  }

data PostReport = PostReport
  { name    :: Maybe [String]
  , text    :: Maybe [String]
  , authors :: Maybe [User]
  }

nonempty' :: Monad m => Validator String m [String]
nonempty' = flopIf (pure . null) ["Must be nonempty"]

atleast' :: Monad m => Int -> Validator String m [String]
atleast' n = flopIf (pure . (==n) . length) ["Must be at least " <> show n <> " characters long"]

checkIf :: Monad m => (i -> m Bool) -> Validator i m e -> Validator i m e -> Validator i m e
checkIf p f t = Validator $ \i -> p i >>= bool (runValidator f i) (runValidator t i)

flopIf :: (Monad m, Monoid e) => (i -> m Bool) -> e -> Validator i m e
flopIf p = checkIf p pass . flop

pass :: (Applicative m, Monoid e) => Validator i m e
pass = Validator $ const $ pure mempty

flop :: Applicative m => e -> Validator i m e
flop = Validator . const . pure . Wrong

--

{-
data Report e v
  = Wrong' e
  | Valid' v

instance (Semigroup e, Semigroup v) => Semigroup (Report e v) where
  Wrong' a <> Wrong' b = Wrong' $ a <> b
  Wrong' e <> Valid' _ = Wrong' e
  Valid' a <> Valid' b = Valid' $ a <> b
  Valid' _ <> Wrong' e = Wrong' e

instance (Semigroup e, Monoid v) => Monoid (Report e v) where
  mempty = Valid' mempty
-}

newtype Valid a = Valid
  { unValid :: a
  }

data Status e = Neutral | Inert e | Wrong e

instance Semigroup e => Semigroup (Status e) where
  Neutral <> s       = Neutral
  s       <> Neutral = Neutral
  Inert b <> Inert d = Inert $ b <> d
  Inert b <> Wrong d = Wrong $ b <> d
  Wrong b <> Inert d = Wrong $ b <> d
  Wrong b <> Wrong d = Wrong $ b <> d

instance Monoid e => Monoid (Status e) where
  mempty = Inert mempty

instance Functor Status where
  fmap _ (Neutral) = Neutral
  fmap f (Inert a) = Inert $ f a
  fmap f (Wrong e) = Wrong $ f e

instance Applicative Status where
  pure = Inert

  (Neutral) <*> s         = Neutral
  s         <*> Neutral   = Neutral
  (Inert f) <*> (Inert e) = Inert $ f e
  (Inert f) <*> (Wrong e) = Wrong $ f e
  (Wrong f) <*> (Inert e) = Wrong $ f e
  (Wrong f) <*> (Wrong e) = Wrong $ f e

instance Alternative Status where
  empty = Neutral

  Neutral <|> s       = Neutral

  Inert e <|> _       = Inert e
  _       <|> Inert e = Inert e

  Wrong _ <|> Neutral = Neutral
  Wrong _ <|> Wrong e = Wrong e

instance Monad Status where
  Neutral >>= _ = Neutral
  Inert e >>= f = f e
  Wrong e >>= f = f e

--

newtype Validator (i :: *) (m :: * -> *) (e :: *) = Validator
  { runValidator :: i -> m (Status e)
  }

instance (Applicative m, Semigroup e) => Semigroup (Validator i m e) where
  Validator b <> Validator d = Validator $ \i -> (<>) <$> b i <*> d i

instance (Applicative m, Monoid e) => Monoid (Validator i m e) where
  mempty = Validator $ const $ pure $ mempty

instance Functor m => Functor (Validator i m) where
  fmap f (Validator v) = Validator $ \i -> (fmap f) <$> v i

instance Applicative m => Applicative (Validator i m) where
  pure = Validator . const . pure . pure

  Validator b <*> Validator d = Validator $ \i -> (<*>) <$> b i <*> d i

instance Applicative m => Alternative (Validator i m) where
  empty = Validator $ const $ pure empty

  Validator b <|> Validator d = Validator $ \i -> (<|>) <$> b i <*> d i

instance Monad m => Monad (Validator i m) where
  Validator vima >>= a_vimb = Validator $ \i -> do
    sa <- vima i
    case sa of
      Neutral -> pure Neutral
      Inert a -> runValidator (a_vimb a) i
      Wrong a -> runValidator (a_vimb a) i

--

check :: (i -> x) -> Validator x m e -> Validator i m e
check sel val = Validator $ runValidator val . sel

checks :: (Traversable t, Applicative m) => (i -> t x) -> Validator x m e -> Validator i m (t e)
checks sel val = Validator $ fmap sequenceA . traverse (runValidator val) . sel

m :: Functor m => Validator i m e -> Validator i m (Maybe e)
m (Validator val) = Validator $ \i -> (fmap Just) <$> val i

e :: Functor m => Validator i m e -> Validator i m (Either e (Valid i))
e (Validator val) = Validator $ \i -> flip fmap (val i) $ \case
  Wrong e -> Wrong $ Left e
  Inert _ -> Inert $ Right $ Valid i
  Neutral -> Inert $ Right $ Valid i

ms :: (Functor m, Functor t) => Validator i m (t e) -> Validator i m (Maybe (t (Maybe e)))
ms val = Validator $ \i -> (fmap $ fmap $ fmap Just) <$> (runValidator (m val) i)
