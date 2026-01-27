{-# OPTIONS_HADDOCK hide #-}

{- |
  Guts of valor.
-}
module Data.Valor.Internal where

import Data.Functor ( (<&>) )


{- |
  Simple wrapper holding a t'Valid' value that has successfully passed the
  validation. It's not supposed to be mapped over, parsed, read, coerced etc.
  (so as to not modify / spoil the t'Valid' value). The only way to construct
  it is by passing an input throug a validator using 'Data.Valor.validateP' or
  'Data.Valor.validateM'.
-}
newtype Valid a = Valid a
  deriving ( Eq , Show )

{- |
  Extract a value from the t'Valid' wrapper for further use / processing.
-}
unValid :: Valid a -> a
unValid ( Valid a ) = a


{- |
  t'Valor' (__VAL__idat__OR__) is the centerpiece of this validation library.
  You can think of it as a function from an input to a possible error.

  Because t'Valor' is essentially just an alias for a function of type
  __@i -> m ( t'Wrong' e )@__ we can think of operations on t'Valor' as
  operations on the resulting t'Wrong' once @i@ has been applied.

  Here's a useful table detailing the behavior of each operation on t'Wrong'
  (and consequently t'Valor'):

  +---------------------------+----------------------------+--------------------------+-------------------+-----------------------+
  |                           | 'Data.Valor.con' / '<>'    | 'Data.Valor.app' / '<*>' | 'Data.Valor.alt'  | 'Data.Valor.acc'      |
  +---------------------------+----------------------------+--------------------------+-------------------+-----------------------+
  | @v'Inert' a × v'Inert' b@ | @v'Inert' $ a '<>' b@      | @v'Inert' $ a b@         | @v'Inert' a@      | @v'Inert' a@          |
  +---------------------------+----------------------------+--------------------------+-------------------+-----------------------+
  | @v'Inert' a × v'Wrong' b@ | @v'Wrong' $ a '<>' b@      | @v'Wrong' $ a b@         | @v'Inert' a@      | @v'Inert' a@          |
  +---------------------------+----------------------------+--------------------------+-------------------+-----------------------+
  | @v'Wrong' a × v'Inert' b@ | @v'Wrong' $ a '<>' b@      | @v'Wrong' $ a b@         | @v'Inert' b@      | @v'Inert' b@          |
  +---------------------------+----------------------------+--------------------------+-------------------+-----------------------+
  | @v'Wrong' a × v'Wrong' b@ | @v'Wrong' $ a '<>' b@      | @v'Wrong' $ a b@         | @v'Wrong' b@      | @v'Wrong' $ a '<>' b@ |
  +---------------------------+----------------------------+--------------------------+-------------------+-----------------------+

  __NOTE:__ You should not directly interact with t'Wrong' as it is only used
  internally in t'Valor'.
-}
newtype Valor i m e = Valor
  { unValor :: i -> m ( Wrong e )
  }

{- |
  Implemented using the t'Wrong' '<>'. Think of it as evaluating the t'Valor'
  and then mappending the resulting t'Wrong's.
-}
instance ( Monad m , Semigroup e ) => Semigroup ( Valor i m e ) where
  Valor b <> Valor d = Valor $ \ i -> (<>) <$> b i <*> d i

{- |
  Implemented using the t'Wrong' 'mempty' wrapped in t'Valor'.
-}
instance ( Monad m , Monoid e ) => Monoid ( Valor i m e ) where
  mempty = Valor $ const $ pure mempty

{- |
  Evaluates the t'Valor' and 'fmap's the @f@ over the resulting t'Wrong'.
-}
instance Monad m => Functor ( Valor i m ) where
  fmap f ( Valor v ) = Valor $ fmap ( fmap f ) . v

{- |
  Evaluates both t'Valor' operands and then does the '<*>' operation on the
  resulting t'Wrong's.
-}
instance Monad m => Applicative ( Valor i m ) where
  pure = Valor . const . pure . pure

  Valor b <*> Valor d = Valor $ \ i -> (<*>) <$> b i <*> d i

{- |
  Evaluates the "input" t'Valor'. If the result is @'Inert' e@ it takes the @e@
  and binds it to get the next t'Valor', however, if the result is @t'Wrong' e@
  it will "remember" that and if the next t'Valor' is 'Inert' it'll be
  converted to v'Wrong'. This will essentially make the whole 'Monad'ic
  computation result in v'Wrong'.
-}
instance Monad m => Monad ( Valor i m ) where
  Valor v >>= evv' = Valor $ \ i -> do
    ve <- v i
    case ve of
      Inert e -> unValor ( evv' e ) i
      Wrong e -> unValor ( evv' e ) i <&> Wrong . valW


{- |
  The internal data type used to accumulate errors and keep track of the error
  state (if there was an actual error or not).
-}
data Wrong e = Inert e | Wrong e
  deriving ( Eq )

{- |
  'Inert' operands are ignored and v'Wrong' operands are 'mappend'ed.
  If both operands are 'Inert' then the first one is ignored. If
  v'Wrong' is one of the operands then the resulting value is also
  v'Wrong'.
-}
instance Semigroup e => Semigroup ( Wrong e ) where
  Inert b <> Inert d = Inert $ b <> d
  Inert b <> Wrong d = Wrong $ b <> d
  Wrong b <> Inert d = Wrong $ b <> d
  Wrong b <> Wrong d = Wrong $ b <> d

{- |
  The 'Monoid's 'mempty' is implemented as @'Inert' 'mempty'@.
-}
instance Monoid e => Monoid ( Wrong e ) where
  mempty = Inert mempty

{- |
  Just a simple 'Functor' instance which applies the function to the value
  within.
-}
instance Functor Wrong where
  fmap f ( Inert e ) = Inert $ f e
  fmap f ( Wrong e ) = Wrong $ f e

{- |
  'Applicative's 'pure' is implemented as 'Inert'. If v'Wrong' is encountered
  in any of the operands then the result will also be v'Wrong'.
-}
instance Applicative Wrong where
  pure = Inert

  Inert f <*> Inert e = Inert $ f e
  Inert f <*> Wrong e = Wrong $ f e

  Wrong f <*> Inert e = Wrong $ f e
  Wrong f <*> Wrong e = Wrong $ f e


{- |
  An alias for the 'mappend' ('<>').
-}
conW :: Semigroup e => Wrong e -> Wrong e -> Wrong e
conW = (<>)

{- |
  An alias for the '<*>'.
-}
appW :: Wrong ( a -> b ) -> Wrong a -> Wrong b
appW = (<*>)

{- |
  Non accumulating 'Control.Applicative.Alternative'. As long as there's one
  'Inert' value the resulting value will be 'Inert'. However, if there are two
  v'Wrong's then only the second one will be returned as a resulting value. If
  there are two 'Inert's then only the first one is returned.
-}
altW :: Wrong e -> Wrong e -> Wrong e
altW ( Inert e ) ( Inert _ ) = Inert e
altW ( Inert e ) ( Wrong _ ) = Inert e
altW ( Wrong _ ) ( Inert e ) = Inert e
altW ( Wrong _ ) ( Wrong e ) = Wrong e

{- |
  Accumulating 'Control.Applicative.Alternative'. Almost the same as 'altW'
  except if there are two v'Wrong's they are 'mappend'ed together.
-}
accW :: Semigroup e => Wrong e -> Wrong e -> Wrong e
accW ( Inert e ) _           = Inert e
accW _           ( Inert e ) = Inert e
accW ( Wrong b ) ( Wrong d ) = Wrong $ b <> d

{- |
  Extracts the value contained within the t'Wrong' regardless if the "internal"
  state is 'Inert' or v'Wrong'.
-}
valW :: Wrong e -> e
valW ( Inert e ) = e
valW ( Wrong e ) = e

{- |
  If the given value is v'Wrong', the first function will be applied, if the
  value is 'Inert' then the second function will be applied.
-}
wrong :: ( e -> a ) -> ( e -> a ) -> Wrong e -> a
wrong _ fi ( Inert e ) = fi e
wrong fw _ ( Wrong e ) = fw e

{- |
  Checks if the value is 'Inert'.
-}
isInert :: Wrong e -> Bool
isInert ( Inert _ ) = True
isInert ( Wrong _ ) = False

{- |
  Checks if the value is v'Wrong'.
-}
isWrong :: Wrong e -> Bool
isWrong ( Inert _ ) = False
isWrong ( Wrong _ ) = True
