module Data.Valor
  ( Valid
  , unValid
  , Valor
  , alt
  , pass
  , pass'
  , fail
  , fail'
  , make
  , test
  , poke
  , toM
  , toE
  , check
  , check1
  , checkN
  , validateP
  , validateM
  )
  where
--
import Prelude hiding ( fail )
--
import Data.Bool ( bool )
import Data.Maybe ( maybe )
import Data.Functor.Identity ( Identity (..) )
import Data.Valor.Internal ( ValorI (..) , altI , valI , isInertI )
--

{-|
  A wrapper indicating that the containing value is indeed a 'Valid' one. You
  are not supposed to be able to construct it your self. You can only obtain it
  by successfully running a 'ValorI'' validator over a value with 'validateP',
  'validateM' or 'toE'.

  You also shouldn't be able to map over it or modify it in any way, as then it
  could no longer be 'Valid'.
-}
newtype Valid a = Valid a
  deriving ( Show )

{-|
  Used to extract a valid value from 'Valid' for further processing.
-}
unValid :: Valid a -> a
unValid ( Valid a ) = a

--

{-|
  The validator or 'ValorI' for short. The core idea of validation (according to
  me) is taking some input and returning an error if it exists.

  With that in mind, you can think of 'ValorI' as a function from some input @i@
  to some possible error @e@. In fact, here's the exact underlying type:

  @i -> m ( 'ValorI' e )@

  'ValorI' is an underlying data type that carries and accumulates the errors.
  You are not supposed to interact with it directly, but since it's helpful to
  know the guts of a system you are working with, here is it's definition:

  @data 'ValorI' e = 'Inert' e | 'Wrong' e@

  'Inert' is here because we sometimes want to inject a value that is not
  necessarily an error (e.g. `pure` from the 'Applicative' interface). If the
  result of validation is 'Inert' then that means there was no error.
-}
newtype Valor i m e = Valor
  { unValor :: i -> m ( ValorI e )
  }

{-|
  The semigroup instance is used to combine multiple validators together. For
  when you e.g. want to check that the user is both over 18 years old, and not
  older than 45.
-}
instance ( Monad m , Semigroup e ) => Semigroup ( Valor i m e ) where
  Valor b <> Valor d = Valor $ \ i -> (<>) <$> (b i) <*> (d i)

{-|
  If your error value @e@ is a monoid then 'mempty' is a @'ValorI' i m e@ that
  always passes. You can use the alias 'pass'.
-}
instance ( Monad m , Monoid e ) => Monoid ( Valor i m e ) where
  mempty = Valor $ const $ pure mempty

{-|
  The good old 'Functor' interface. If you want to find out if there's an error
  before you run 'validateP' or 'validateM', instead of mapping something like
  'Just' to get the 'Valor i m ( Maybe e )' use the 'toM' or one of the @checkM@
  functions, as they will return 'Nothing' for when validation is 'Inert' and
  @'Just' e@ in case of an error.
-}
instance ( Monad m ) => Functor ( Valor i m ) where
  fmap f ( Valor v ) = Valor $ \ i -> ( fmap f ) <$> ( v i )

{-|
  The 'Applicative' interface is important for building more complex and
  structured error values. It is also prefered to the 'Monad' interface.
-}
instance ( Monad m ) => Applicative ( Valor i m ) where
  pure = Valor . const . pure . pure

  ( Valor b ) <*> ( Valor d ) = Valor $ \ i -> (<*>) <$> ( b i ) <*> ( d i )

{-|
  While the 'Monad' instance is valid and quite powerful, it is recommended that
  you use the 'Applicative' interface werever possible to avoid shooting your
  self to the foot. While using the 'Monad' interface, __you__ are the one
  responsible for how the errors aggregate, so be careful.
-}
instance ( Monad m ) => Monad ( Valor i m ) where
  b >>= bd = Valor $ \ i -> do
    vb <- unValor b i
    vd <- unValor ( bd $ valI vb ) i
    pure $ case ( vb , vd ) of
      ( Inert _ , Inert e ) -> Inert e
      ( Inert _ , Wrong e ) -> Wrong e
      ( Wrong _ , Inert e ) -> Wrong e
      ( Wrong _ , Wrong e ) -> Wrong e

--

{-|
  Unfortunately it is not possible to create the 'Alternative' instance for
  'Valor'. To rectify this, here's the 'alt' function which behaves like what
  one might expect from the 'Alternative's '<|>'.
-}
alt :: ( Monad m , Semigroup e ) => Valor i m e -> Valor i m e -> Valor i m e
alt ( Valor b ) ( Valor d ) = Valor $ \ i -> altI <$> ( b i ) <*> ( d i )

--

{-|
  An alias for 'mempty'. Basically, it's a validator that always passes. If your
  error is not a 'Monoid' then you can use 'pure' from the 'Applicative' to
  inject a "dummy" 'Inert' value.
-}
pass :: ( Monad m , Monoid e ) => Valor i m e
pass = mempty

{-|
  If you want to inspect the input value before constructing a validator that
  always passes, use this. It might come in handy when using the 'Applicative'
  interface.
-}
pass' :: Monad m => ( i -> m e ) -> Valor i m e
pass' s = Valor $ fmap Inert . s

{-|
  Constructs a validator that always fails with the error @e@.
-}
fail :: Monad m => e -> Valor i m e
fail = Valor . const . pure . Wrong

{-|
  Inspect the input value before constructing an error to fail with.
-}
fail' :: Monad m => ( i -> m e ) -> Valor i m e
fail' s = Valor $ fmap Wrong . s

{-|
  Inspect the input and then succeed if the result of the inspection is
  'Nothing' or fail with @e@ if the result is @'Just' e@.
-}
make :: ( Monad m , Monoid e ) => ( i -> Maybe e ) -> Valor i m e
make t = Valor $ \ i -> flip unValor i $ maybe pass fail $ t i

{-|
  Use one or the other validator depending on the result of the test.
-}
test
  :: Monad m
  => Valor i m e      -- ^ validator to use if 'False'
  -> Valor i m e      -- ^ validator to use if 'True'
  -> ( i -> m Bool )  -- ^ function for inspecting the input
  -> Valor i m e
test f p t = Valor $ \ i -> t i >>= flip unValor i . bool f p

{-|
  Use one or the other validator depending on the result of the primary
  validator. (error of the primary validator is not aggregated)
-}
poke
  :: Monad m
  => Valor i m e -- ^ validator to run when primary fails
  -> Valor i m e -- ^ validator to run when primary succeeds
  -> Valor i m x -- ^ primary validator
  -> Valor i m e
poke f p t = Valor $ \ i -> do
  vt <- unValor t i
  flip unValor i $ case vt of
    Wrong _ -> f
    Inert _ -> p

--

{-|
  It converts the 'ValorI' value so that if the internal state is 'Inert'
  'Nothing' is returned, and if the internal state is 'Wrong' then @'Just' e@ is
  returned.

  This can be useful in conjunction with the 'Monad' interface as it allows you
  to know if the error has occurred during the validation process.
-}
toM :: Monad m => Valor i m e -> Valor i m ( Maybe e )
toM ( Valor v ) = Valor $ \ i -> do
  vv <- v i
  pure $ case vv of
    Inert _ -> Inert Nothing
    Wrong e -> Wrong $ Just e

{-|
  It modifies the 'ValorI' value so that if the internal state is 'Inert' it
  returns the 'Valid' input value @i@ and error @e@ otherwise.

  It might come in handy if you want to aggregate the inputs alongside errors.
-}
toE :: Monad m => Valor i m e -> Valor i m ( Either ( Valid i ) e )
toE ( Valor v ) = Valor $ \ i -> do
  vv <- v i
  pure $ case vv of
    Inert _ -> Inert $ Left $ Valid i
    Wrong e -> Wrong $ Right e

--

check :: Monad m => ( i -> x ) -> Valor x m e -> Valor i m e
check s v = Valor $ \ i -> unValor v ( s i )

{-|
  This is useful when you want to build up a more complex and structured error
  value where some fields can succeed and some can fail. It makes an existing
  'ValorI' work on a value of the field of a particular input @i@.
-}
check1
  :: Monad m
  => ( i -> x )   -- ^ field selector
  -> Valor x m e  -- ^ validator
  -> Valor i m ( Maybe e )
check1 s = toM . check s

{-|
  Similar to 'check1', however it traverses a given validator over every item in
  a 'Traversable' such as 'List'. If there is even a single error, it returns
  'Just' a 'Traversable' with possible errors for each value and 'Nothing'
  otherwise.

  Very useful for validating a record field that contains a e.g. list of items.
-}
checkN :: ( Monad m , Traversable t ) => ( i -> t x ) -> Valor x m e -> Valor i m ( Maybe ( t ( Maybe e ) ) )
checkN s v = toM $ checkN' s ( toM v )
  where
    checkN' :: ( Monad m , Traversable t ) => ( i -> t x ) -> Valor x m e -> Valor i m ( t e )
    checkN' s v = Valor $ \ i -> do
      es <- traverse ( unValor v ) ( s i )
      pure $ bool Wrong Inert (all isInertI es) $ fmap valI es

--

{-|
  A "pure" validation performed within the 'Identity' monad. Use if you do not
  have to access e.g. your database to check if the username is free or not.
-}
validateP :: Valor i Identity e -> i -> Either ( Valid i ) e
validateP ( Valor v ) i = case runIdentity ( v i ) of
  Inert _ -> Left $ Valid i
  Wrong e -> Right e

{-|
  An "impure" validation performed within the user provided monad @m@. Use if
  you have to perform some side effects during the validation process.
-}
validateM :: Monad m => Valor i m e -> i -> m ( Either ( Valid i ) e )
validateM ( Valor v ) i = do
  vi <- v i
  pure $ case vi of
    Inert _ -> Left  $ Valid i
    Wrong e -> Right $ e
