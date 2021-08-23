{- |
  The idea behind Valor was to provide a simple but powerful data validation
  library that is easy to understand quickly.

  It achieves these goals by providing the 'Applicative' and 'Monad' instances
  along with very few, well documented, core combinators. This allows you to
  figure out what's important and to create your own purpose specific
  combinators as you need them, instead of searching through a plethora of
  predefined combinators whose naming scheme might not match your intuition.

  In case you want to better understand what's going under the hood, check out
  the "Data.Valor.Internal" module. While you can't include it directly, it is
  provided here as a learning aid.
-}
module Data.Valor
  ( {- * Core -}
    {- | Core data types used in the validation process. -}

    {- ** Valid -}
    Valid
  , unValid

    {- ** Valor -}
  , Valor

    {- * Make -}
    {- | Utilities for making validators. -}

    {- ** Operations -}
  , con
  , app
  , alt
  , acc

    {- ** Primitives -}
  , pass
  , fail

    {- ** Constructors -}
  , test
  , make
  , peek
  , poke

    {- * Modify -}
    {- | Functions used to modify the behavior of validators. -}
  , nerf
  , peer
  , adapt
  , check1
  , checkN

    {- * Validate -}
    {- | Functions used to apply your validators to the data. -}
  , validateP
  , validateM
  )
  where
--
import Prelude hiding ( fail )
--
import Data.Bool ( bool )
import Data.Valor.Internal ( Valid (..) , unValid , Valor (..) , Wrong (..) , altW , accW , valW , wrong , isInert )
import Data.Functor.Identity ( Identity (..) )
--

{- |
  An alias for 'mappend' (<>).
-}
con :: ( Monad m , Semigroup e ) => Valor i m e -> Valor i m e -> Valor i m e
con = (<>)

{- |
  An alias for '<*>'.
-}
app :: ( Monad m ) => Valor i m (a -> b) -> Valor i m a -> Valor i m b
app = (<*>)

{- |
  As an alternative to the 'Alternative' type class and the '<|>' operator 'alt'
  is provided. It will result in an error only if both arguments are
  'Wrong.Wrong', however, only the last error will be returned.
-}
alt :: Monad m => Valor i m e -> Valor i m e -> Valor i m e
alt ( Valor b ) ( Valor d ) = Valor $ \ i -> altW <$> ( b i ) <*> ( d i )

{- |
  Accumulating version of 'alt' where if both operands are 'Wrong.Wrong' they
  will be 'mappend'ed.
-}
acc :: ( Monad m , Semigroup e ) => Valor i m e -> Valor i m e -> Valor i m e
acc ( Valor b ) ( Valor d ) = Valor $ \ i -> accW <$> ( b i ) <*> ( d i )

--

{- |
  A validator that always 'pass'es the test. Essentially just an alias for
  'mempty'. If you want to create a validator that always passes for a type that
  isn't a 'Monoid', then you can use 'pure', however you will have to provide it
  a "dummy" error value that you yourself will manage as "neutral".
-}
pass :: ( Monad m , Monoid e ) => Valor i m e
pass = mempty

{- |
  Constructs a validator that always fails with provided error @e@.
-}
fail :: Monad m => e -> Valor i m e
fail = Valor . const . pure . Wrong

--

{- |
  Apply one or the other validator depending on the result of a test.
-}
test
  :: Monad m
  => Valor i m e      -- ^ validator to use on 'False'
  -> Valor i m e      -- ^ validator to use on 'True'
  -> ( i -> m Bool )  -- ^ a predicate
  -> Valor i m e
test ( Valor f ) ( Valor p ) t = Valor $ \ i -> t i >>= bool ( f i ) ( p i )

{- |
  Construct a validator that checks the input @i@ and 'Maybe' results in an
  error @e@.
-}
make :: ( Monad m , Monoid e ) => ( i -> m ( Maybe e ) ) -> Valor i m e
make ime = Valor $ \ i -> ime i >>= flip unValor i . maybe pass fail

{- |
  Construct a validator that applies another validator depending on the result
  from a test validator. If both the "test" and the "fail" validator fail, then
  only the error from the "fail" validator is returned.
-}
peek :: ( Monad m , Semigroup e ) => Valor i m e -> Valor i m e -> Valor i m e -> Valor i m e
peek ( Valor f ) ( Valor p ) ( Valor t ) = Valor $ \ i -> t i >>= wrong ( const $ f i ) ( const $ p i )

{- |
  Just like 'peek', except if both the "test" and the "fail" validators fail,
  their results are 'mappend'ed ('<>').
-}
poke :: ( Monad m , Semigroup e ) => Valor i m e -> Valor i m e -> Valor i m e -> Valor i m e
poke ( Valor f ) ( Valor p ) ( Valor t ) = Valor $ \ i -> do
  tr <- t i
  case tr of
    Inert _ -> p i
    Wrong b -> do
      tr' <- f i
      pure $ case tr' of
        Inert _ -> Wrong b
        Wrong d -> Wrong $ b <> d

--

{- |
  If a validator fails with an error 'nerf' will make that error 'Wrong.Inert'
  essentially making it pass.

  Use of this function is discouraged, however it might come in handy in
  combination with 'peer' within the 'Monad'ic context when you want to check
  the result of a validation without failing the whole computation.

  Be careful though, @nerf . peer@ is not the same as @peer . nerf@ (which is
  essentially useless and will always result in 'Nothing').
-}
nerf :: Monad m => Valor i m e -> Valor i m e
nerf ( Valor v ) = Valor $ \ i -> v i >>= pure . Inert . valW

{- |
  Allows you to 'peer' into the 'Wrong' contained within the 'Valor' (how
  poetic) and if there is nothing 'Wrong.Wrong' it will return 'Nothing'.
-}
peer :: Monad m => Valor i m e -> Valor i m ( Maybe e )
peer ( Valor v ) = Valor $ \ i -> v i >>= pure . wrong ( Wrong . Just ) ( const $ Inert Nothing )

--

{- |
  It can 'adapt' a validator to the new input type given a conversion function,
  making it useful for working with records (think field selectors).

  This is essentially a 'Data.Functor.Contravariant.contramap' from
  "Data.Functor.Contravariant", however, due to the placement of arguments in
  the 'Valor' type constructor it is not possible to write that instance.
-}
adapt :: Monad m => ( i -> x ) -> Valor x m e -> Valor i m e
adapt s ( Valor v ) = Valor $ v . s

{- |
  Useful for constructing structured errors / error records. By using 'Maybe'
  you can specify for which exact field an error has occurred. It is implemented
  using 'peer' and 'adapt'.
-}
check1 :: Monad m => ( i -> x ) -> Valor x m e -> Valor i m ( Maybe e )
check1 s = peer . adapt s

{- |
  Similar to 'check1', except it will apply a validator to each element of a
  'Traversable', e.g. a list. If every element of a list is valid, then we get
  'Nothing', otherwise we get a list of 'Maybe's for each validated value.

  This allows us to know in which exact element of a list an error has occurred
  (if you trust your 'Traversable' to maintain the original order after the
  traversal).
-}
checkN :: ( Monad m , Traversable t ) => ( i -> t x ) -> Valor x m e -> Valor i m ( Maybe ( t ( Maybe e ) ) )
checkN s v = Valor $ \ i -> do
  ws <- traverse ( unValor $ peer v ) ( s i )
  pure $ if all isInert ws
    then Inert $ Nothing
    else Wrong $ Just $ fmap valW ws

--

{- |
  Runs a validator within the 'Identity' 'Monad', essentially making it a "pure"
  validation.
-}
validateP :: Valor i Identity e -> i -> Either ( Valid i ) e
validateP v = runIdentity . validateM v

{- |
  Runs a validator within the user provided 'Monad' @m@ allowing you to perform
  side effects during the validation, e.g. check with the application database
  if the username is already registered.
-}
validateM :: Monad m => Valor i m e -> i -> m ( Either ( Valid i ) e )
validateM ( Valor v ) i = v i >>= pure . wrong Right ( const $ Left $ Valid i )
