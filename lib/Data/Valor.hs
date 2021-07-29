module Data.Valor where
--
import Data.Bifunctor (Bifunctor (..))
import Control.Applicative (Alternative (..))
--

newtype Valid a = Valid
  { unValid :: a
  }

instance Semigroup a => Semigroup (Valid a) where
  Valid b <> Valid d = Valid $ b <> d

instance Monoid a => Monoid (Valid a) where
  mempty = Valid mempty

instance Functor Valid where
  fmap f (Valid a) = Valid $ f a

instance Applicative Valid where
  pure = Valid
  Valid f <*> Valid a = Valid $ f a

--

data Report e
  = Empty'
  | Amiss'
  | Inert' e
  | Wrong' e

instance Semigroup e => Semigroup (Report e) where
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

instance Monoid e => Monoid (Report e) where
  mempty = Inert' mempty

instance Functor Report where
  fmap _ Empty'     = Empty'
  fmap f (Inert' e) = Inert' $ f e
  fmap f (Wrong' e) = Wrong' $ f e

instance Applicative Report where
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


instance Alternative Report where
  empty = Empty'

  Empty'   <|> Inert' e   = Inert' e
  Empty'   <|> _          = Empty'

  Amiss'   <|> r          = r

  Inert' e <|> _          = Inert' e

  Wrong' e <|> Empty'     = Empty'
  Wrong' e <|> Amiss'     = Wrong' e
  Wrong' _ <|> Inert' e   = Inert' e
  Wrong' _ <|> Wrong' e   = Wrong' e

instance Monad Report where
  re >>= e_rn = case re of
    Empty'   -> Empty'
    Amiss'   -> Amiss'
    Inert' e -> e_rn e
    Wrong' e -> e_rn e

--

newtype Validator m v e = Validator
  { runValidator :: v -> m (Report e)
  }

instance (Applicative m, Semigroup v, Semigroup e) => Semigroup (Validator m v e) where
  Validator b <> Validator d = Validator $ \v -> (<>) <$> (b v) <*> (d v)

instance (Applicative m, Semigroup v, Monoid e) => Monoid (Validator m v e) where
  mempty = Validator $ const $ pure mempty

instance Functor m => Functor (Validator m v) where
  fmap f (Validator val) = Validator $ \v -> (fmap f) <$> (val v)

instance Applicative m => Applicative (Validator m v) where
  pure = Validator . const . pure . pure
  Validator b <*> Validator d = Validator $ \v -> (<*>) <$> (b v) <*> (d v)

instance Monad m => Monad (Validator m v) where
  Validator ve >>= e_vn = Validator $ \v -> do
    re <- ve v
    case re of
      Empty'   -> pure Empty'
      Amiss'   -> pure Amiss'
      Inert' e -> runValidator (e_vn e) v
      Wrong' e -> runValidator (e_vn e) v
