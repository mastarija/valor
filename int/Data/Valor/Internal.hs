module Data.Valor.Internal where
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
