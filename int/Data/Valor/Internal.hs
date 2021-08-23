module Data.Valor.Internal where
--

{- |
  Simple wrapper holding a 'Valid' value that has successfully passed the
  validation. It's not supposed to be mapped over, parsed, read, coerced etc.
  (so as to not modify / spoil the 'Valid' value). The only way to construct it
  is by passing an input throug a validator using 'Data.Valor.validateP' or
  'Data.Valor.validateM'.
-}
newtype Valid a = Valid a
  deriving ( Eq , Show )

{- |
  Extract a value from the 'Valid' wrapper for further use / processing.
-}
unValid :: Valid a -> a
unValid ( Valid a ) = a

--

{- |
  'Valor' (__VAL__idat__OR__) is the centerpiece of this validation library. You
  can think of it as a function from an input to a possible error. Do check the
  details in the "Data.Valor.Internal" module.

  Because 'Valor' is essentially just an alias for a function of type
  __@i -> m ('Wrong' e)@__ we can think of operations on 'Valor' as operations
  on the resulting 'Wrong' once @i@ has been applied.

  Here's a useful table detailing the behavior of each operation on 'Wrong'
  (and consequently 'Valor'):

  +-----------------------------------+----------------------------+--------------------------+-------------------+----------------------------+
  |                                   | 'Data.Valor.con' / '<>'    | 'Data.Valor.app' / '<*>' | 'Data.Valor.alt'  | 'Data.Valor.acc'           |
  +-----------------------------------+----------------------------+--------------------------+-------------------+----------------------------+
  |@'Wrong.Inert' a × 'Wrong.Inert' b@| @'Wrong.Inert' b@          | @'Wrong.Inert' $ a b@    | @'Wrong.Inert' a@ | @'Wrong.Inert' a@          |
  +-----------------------------------+----------------------------+--------------------------+-------------------+----------------------------+
  |@'Wrong.Inert' a × 'Wrong.Wrong' b@| @'Wrong.Wrong' b@          | @'Wrong.Wrong' $ a b@    | @'Wrong.Inert' a@ | @'Wrong.Inert' a@          |
  +-----------------------------------+----------------------------+--------------------------+-------------------+----------------------------+
  |@'Wrong.Wrong' a × 'Wrong.Inert' b@| @'Wrong.Wrong' a@          | @'Wrong.Wrong' $ a b@    | @'Wrong.Inert' b@ | @'Wrong.Inert' b@          |
  +-----------------------------------+----------------------------+--------------------------+-------------------+----------------------------+
  |@'Wrong.Wrong' a × 'Wrong.Wrong' b@| @'Wrong.Wrong' $ a '<>' b@ | @'Wrong.Wrong' $ a b@    | @'Wrong.Wrong' b@ | @'Wrong.Wrong' $ a '<>' b@ |
  +-----------------------------------+----------------------------+--------------------------+-------------------+----------------------------+

  __NOTE:__ You can not directly interact with 'Wrong' as it is only used
  internally in 'Valor'.
-}
newtype Valor i m e = Valor
  { unValor :: i -> m ( Wrong e )
  }

{- |
  Implemented using the 'Wrong' '<>'. Think of it as evaluating the 'Valor' and
  then mappending the resulting 'Wrong's.
-}
instance ( Monad m , Semigroup e ) => Semigroup ( Valor i m e ) where
  Valor b <> Valor d = Valor $ \ i -> (<>) <$> ( b i ) <*> ( d i )

{- |
  Implemented using the 'Wrong' 'mempty' wrapped in 'Valor'.
-}
instance ( Monad m , Monoid e ) => Monoid ( Valor i m e ) where
  mempty = Valor $ const $ pure mempty

{- |
  Evaluates the 'Valor' and 'fmap's the @f@ over the resulting 'Wrong'.
-}
instance Monad m => Functor ( Valor i m ) where
  fmap f ( Valor v ) = Valor $ \ i -> fmap f <$> v i

{- |
  Evaluates both 'Valor' operands and then does the '<*>' operation on the
  resulting 'Wrong's.
-}
instance Monad m => Applicative ( Valor i m ) where
  pure = Valor . const . pure . pure

  Valor b <*> Valor d = Valor $ \ i -> (<*>) <$> ( b i ) <*> ( d i )

{- |
  Evaluates the "input" 'Valor'. If the result is @'Inert' e@ it takes the @e@
  and binds it to get the next 'Valor', however, if the result is @'Wrong' e@ it
  will "remember" that and if the next 'Valor' is 'Inert' it'll be converted to
  'Wrong.Wrong'.
-}
instance Monad m => Monad ( Valor i m ) where
  Valor v >>= evv' = Valor $ \ i -> do
    ve <- v i
    case ve of
      Inert e -> unValor ( evv' e ) i
      Wrong e -> unValor ( evv' e ) i >>= pure . Wrong . valW

--

{- |
  The internal data type used to accumulate errors and keep track of the error
  state (if there was an actual error or not).
-}
data Wrong e = Inert e | Wrong e

{- |
  'Wrong.Inert' operands are ignored and 'Wrong.Wrong' operands are 'mappend'ed.
  If both operands are 'Inert.Inert' then the first one is ignored. If
  'Wrong.Wrong' is one of the operands then the resulting value is also
  'Wrong.Wrong'.
-}
instance Semigroup e => Semigroup ( Wrong e ) where
  Inert _ <> x       = x
  x       <> Inert _ = x
  Wrong b <> Wrong d = Wrong $ b <> d

{- |
  The 'Monoid's 'mempty' is implemented as @'Wrong.Inert' 'mempty'@.
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
  'Applicative's 'pure' is implemented as 'Wrong.Inert'. If 'Wrong.Wrong' is
  encountered in any of the operands then the result will also be 'Wrong'.
-}
instance Applicative Wrong where
  pure = Inert

  Inert f <*> Inert e = Inert $ f e
  Inert f <*> Wrong e = Wrong $ f e

  Wrong f <*> Inert e = Wrong $ f e
  Wrong f <*> Wrong e = Wrong $ f e

{- |
  This monad instance will detect the 'Wrong.Wrong' value and propagate it to
  the final result. Instance is not very useful since this is an internal
  library, and exists simply because it can. :P
-}
instance Monad Wrong where
  Inert e >>= ewe' = ewe' e
  Wrong e >>= ewe' = case ewe' e of
    Inert e' -> Wrong e'
    Wrong e' -> Wrong e'

--

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
  Non accumulating 'Alternative'. As long as there's one 'Wrong.Inert' value the
  resulting value will be 'Inert'. However, if there are two 'Wrong's then only
  the second one will be returned as a resulting value. If there are two
  'Inert's then only the first one is returned.
-}
altW :: Wrong e -> Wrong e -> Wrong e
altW ( Inert e ) _           = Inert e
altW _           ( Inert e ) = Inert e
altW ( Wrong _ ) x           = x

{- |
  Accumulating 'Alternative'. Almost the same as 'altW' except if there are two
  'Wrong.Wrong's they are 'mappend'ed together.
-}
accW :: Semigroup e => Wrong e -> Wrong e -> Wrong e
accW ( Inert e ) _           = Inert e
accW _           ( Inert e ) = Inert e
accW ( Wrong b ) ( Wrong d ) = Wrong $ b <> d

{- |
  Extracts the value contained within the 'Wrong' regardless if the "internal"
  state is 'Wrong.Inert' or 'Wrong.Wrong'.
-}
valW :: Wrong e -> e
valW ( Inert e ) = e
valW ( Wrong e ) = e

{- |
  If the given value is 'Wrong.Wrong', the first function will be applied, if
  the value is 'Wrong.Inert' then the second function will be applied.
-}
wrong :: ( e -> a ) -> ( e -> a ) -> Wrong e -> a
wrong _ fi ( Inert e ) = fi e
wrong fw _ ( Wrong e ) = fw e

{- |
  Checks if the value is 'Wrong.Inert'.
-}
isInert :: Wrong e -> Bool
isInert ( Inert _ ) = True
isInert ( Wrong _ ) = False

{- |
  Checks if the value is 'Wrong.Wrong'.
-}
isWrong :: Wrong e -> Bool
isWrong ( Inert _ ) = False
isWrong ( Wrong _ ) = True
