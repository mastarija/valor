module Data.Valor.Internal where
--

{-|
  The core internal datatype that is carrying and accumulating the errors.
-}
data ValorI e
  = Inert e
  | Wrong e

--

{-|
  'Inert' values are always ignored (unless there are two, then the second one
  is returned). This is because they are essentially just a placeholder.
-}
instance Semigroup e => Semigroup ( ValorI e ) where
  Inert _ <> v = v
  Wrong e <> Inert _ = Wrong e
  Wrong b <> Wrong d = Wrong $ b <> d

{-|
  'ValorI' is a 'Monoid' only if the inner error type is a monoid.
-}
instance Monoid e => Monoid ( ValorI e ) where
  mempty = Inert mempty

{-|
  Simple and clean 'Functor' instance applying the function over the inner
  value.
-}
instance Functor ValorI where
  fmap f ( Inert e ) = Inert $ f e
  fmap f ( Wrong e ) = Wrong $ f e

{-|
  The 'pure' is defined as 'Inert'. This is because we don't want to introduce
  an error immediately if we just want to apply e.g. a constructor over the
  'Applicative' values.
-}
instance Applicative ValorI where
  pure = Inert

  Inert f <*> Inert e = Inert $ f e
  Inert f <*> Wrong e = Wrong $ f e
  Wrong f <*> Inert e = Wrong $ f e
  Wrong f <*> Wrong e = Wrong $ f e

--

{-|
  Implementation of the 'Alternative' operator without the 'empty'. If we could
  limit the internal type of the 'Alternative' type class, there would be a
  viable 'empty' and thus an 'Alternative' instance. Buuut... since we can't,
  this is the best we can do without introducing more complexity.
-}
altI :: Semigroup e => ValorI e -> ValorI e -> ValorI e
altI ( Inert e ) _ = Inert e
altI _ ( Inert e ) = Inert e
altI ( Wrong b ) ( Wrong d ) = Wrong $ b <> d

--

{-|
  Gives back the inner error value, regardless if it's 'Inert' or 'Wrong'.
-}
valI :: ValorI e -> e
valI ( Inert e ) = e
valI ( Wrong e ) = e

{-|
  Checks if the 'ValorI' value is 'Inert'.
-}
isInertI :: ValorI e -> Bool
isInertI ( Inert _ ) = True
isInertI ( Wrong _ ) = False

--
