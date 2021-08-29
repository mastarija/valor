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

  Also, do check the __TUTORIAL__ at the bottom.
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
  , passIf
  , passIfM
  , fail
  , failIf
  , failIfM

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

    {- * Tutorial -}
    -- $tutorial
  )
  where
--
import Prelude hiding ( fail )
--
import Data.Bool ( bool )
import Data.Valor.Internal ( Valid (..) , unValid , Valor (..) , Wrong (..) , altW , accW , valW , wrong , isInert )
import Data.Functor.Identity ( Identity (..) )
--
import Data.List.NonEmpty ( NonEmpty (..) )
--

{- |
  An alias for 'mappend' (<>).
-}
con :: ( Monad m , Semigroup e ) => Valor i m e -> Valor i m e -> Valor i m e
con = (<>)

{- |
  An alias for '<*>'.
-}
app :: Monad m => Valor i m (a -> b) -> Valor i m a -> Valor i m b
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

  ==== __Example__

  >>> validateP pass 1
  Left (Valid 1)

-}
pass :: ( Monad m , Monoid e ) => Valor i m e
pass = mempty

{- |
  A validator that fails with 'e' if the predicate returns 'False'.

  ==== __Example__

  >>> validateP ( passIf "must be greater than 0" (>0) ) 1
  Left (Valid 1)

  >>> validateP ( passIf "must be greater than 0" (>0) ) 0
  Right "must be greater than 0"
-}
passIf :: ( Monad m , Monoid e ) => e -> ( i -> Bool ) -> Valor i m e
passIf e p = passIfM e ( pure . p )

{- |
  A monadic version of 'passIf'.
-}
passIfM :: ( Monad m , Monoid e ) => e -> ( i -> m Bool ) -> Valor i m e
passIfM e = test ( fail e ) pass

{- |
  Constructs a validator that always fails with provided error @e@.

  ==== __Example__

  >>> validateP ( fail "YOU SHALL NOT PASS!!!" ) 1
  Right "YOU SHALL NOT PASS!!!"

-}
fail :: Monad m => e -> Valor i m e
fail = Valor . const . pure . Wrong

{- |
  A validator that fails with 'e' if the predicate returns 'True'.

  ==== __Example__

  >>> validateP ( failIf "must be less than or equal to 0" (>0) ) 1
  Right "must be less than or equal to 0"

  >>> validateP ( failIf "must be less than or equal to 0" (>0) ) (-20)
  Left (Valid (-20))
-}
failIf :: ( Monad m , Monoid e ) => e -> ( i -> Bool ) -> Valor i m e
failIf e p = failIfM e ( pure . p )

{- |
  A monadic version of 'failIf'.
-}
failIfM :: ( Monad m , Monoid e ) => e -> ( i -> m Bool ) -> Valor i m e
failIfM e p = test pass ( fail e ) p

--

{- |
  Apply one or the other validator depending on the result of a test.

  ==== __Example__

  >>> let exV = test pass (fail "I'm a failure") (pure . (>3))

  >>> validateP exV 3
  Left (Valid 3)

  >>> validateP exV 4
  Right "I'm a failure"
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

  ==== __Example__

  >>> let exV = make $ \ i -> pure $ if i > 3 then Nothing else Just "I'm 3 or less failure"

  >>> validateP exV 3
  Right "I'm 3 or less failure"

  >>> validateP exV 4
  Left (Valid 4)
-}
make :: ( Monad m , Monoid e ) => ( i -> m ( Maybe e ) ) -> Valor i m e
make ime = Valor $ \ i -> ime i >>= flip unValor i . maybe pass fail

{- |
  Construct a validator that applies another validator depending on the result
  from a test validator. If both the "test" and the "fail" validator fail, then
  only the error from the "fail" validator is returned.

  ==== __Example__

  >>> let failV = failIf "I'm less than 3" (<3)
  >>> let passV = failIf "I'm greater than 4" (>4)
  >>> let testV = failIf "I'm not divisible by 2" odd
  >>> let exV = peek failV passV testV

  >>> validateP exV 7
  Left (Valid 7)

  >>> validateP exV 6
  Right "I'm greater than 4"

  >>> validateP exV 2
  Left (Valid 2)

  >>> validateP exV 1
  Right "I'm less than 3"
-}
peek :: ( Monad m , Semigroup e ) => Valor i m e -> Valor i m e -> Valor i m e -> Valor i m e
peek ( Valor f ) ( Valor p ) ( Valor t ) = Valor $ \ i -> t i >>= wrong ( const $ f i ) ( const $ p i )

{- |
  Just like 'peek', except if both the "test" and the "fail" validators fail,
  their results are 'mappend'ed ('<>').

  ==== __Example__

  >>> let failV = failIf ["I'm less than 3"] (<3)
  >>> let passV = failIf ["I'm greater than 4"] (>4)
  >>> let testV = failIf ["I'm not divisible by 2"] odd
  >>> let exV = poke failV passV testV

  >>> validateP exV 7
  Left (Valid 7)

  >>> validateP exV 6
  Right ["I'm greater than 4"]

  >>> validateP exV 2
  Left (Valid 2)

  >>> validateP exV 1
  Right ["I'm not divisible by 2","I'm less than 3"]
-}
poke :: ( Monad m , Semigroup e ) => Valor i m e -> Valor i m e -> Valor i m e -> Valor i m e
poke ( Valor f ) ( Valor p ) ( Valor t ) = Valor $ \ i -> do
  tr <- t i
  case tr of
    Inert _ -> p i
    Wrong b -> do
      tr' <- f i
      pure $ case tr' of
        Inert e -> Inert e
        Wrong d -> Wrong $ b <> d

--

{- |
  If a validator fails with an error 'nerf' will make that error 'Wrong.Inert'
  essentially making it pass.

  Use of this function is discouraged, however it might come in handy in
  combination with 'peer' within the 'Monad'ic context when you want to check
  the result of a validation without failing the whole 'Monad'ic computation.

  Be careful though, @nerf . peer@ is not the same as @peer . nerf@ (which is
  essentially useless and will always result in 'Nothing').

  ==== __Example__

  >>> validateP (nerf $ fail "I'm an error that will never appear") 0
  Left (Valid 0)
-}
nerf :: Monad m => Valor i m e -> Valor i m e
nerf ( Valor v ) = Valor $ \ i -> v i >>= pure . Inert . valW

{- |
  Allows you to 'peer' into the 'Wrong' contained within the 'Valor' (how
  poetic) and if there is nothing 'Wrong.Wrong' it will return 'Nothing'.

  It might be useful in the 'Monad'ic context to know if the validator has
  failed (in which case @'Just' e@ is returned) or if it has succeeded.

  ==== __Example__

  >>> validateP (peer $ fail "I have failed") 0
  Right (Just "I have failed")

  >>> validateP (peer pass) 0
  Left (Valid 0)

  >>> let exV = peer (failIf "I'm less than 3" (<3)) >>= maybe (fail "I fail if previous validator succeeds") fail

  >>> validateP exV 3
  Right "I fail if previous validator succeeds"

  >>> validateP exV 2
  Right "I'm less than 3"
-}
peer :: Monad m => Valor i m e -> Valor i m ( Maybe e )
peer ( Valor v ) = Valor $ \ i -> v i >>= pure . wrong ( Wrong . Just ) ( const $ Inert Nothing )

--

{- |
  It can 'adapt' a validator to the new input type given a conversion function,
  making it useful for working with records (think field selectors) or newtypes.

  This is essentially a 'Data.Functor.Contravariant.contramap' from
  "Data.Functor.Contravariant", however, due to the placement of arguments in
  the 'Valor' type constructor it is not possible to write that instance.

  ==== __Example__

  >>> newtype Age = Age { unAge :: Int } deriving Show

  >>> validateP (adapt unAge $ failIf "under aged" (<18)) (Age 78)
  Left (Valid (Age {unAge = 78}))

  >>> validateP (adapt unAge $ failIf "under aged" (<18)) (Age 14)
  Right "under aged"
-}
adapt :: Monad m => ( i -> x ) -> Valor x m e -> Valor i m e
adapt s ( Valor v ) = Valor $ v . s

{- |
  Useful for constructing structured errors / error records. By using 'Maybe'
  you can specify for which exact field an error has occurred. It is implemented
  using 'peer' and 'adapt'.

  ==== __Example__

  >>> data ID = ID {unID :: Int} deriving Show
  >>> data User = User {userID :: ID, userName :: String} deriving Show
  >>> data UserError = UserError {ueID :: Maybe [String], ueName :: Maybe [String]} deriving Show

  >>> userValidator = UserError <$> check1 (unID . userID) (passIf ["invalid ID"] (>0)) <*> check1 userName (failIf ["username can't be empty"] null)

  >>> validateP userValidator $ User (ID (-1)) ""
  Right (UserError {ueID = Just ["invalid ID"], ueName = Just ["username can't be empty"]})

  >>> validateP userValidator $ User (ID 0) "username"
  Right (UserError {ueID = Just ["invalid ID"], ueName = Nothing})

  >>> validateP userValidator $ User (ID 11) "mastarija"
  Left (Valid (User {userID = ID {unID = 11}, userName = "mastarija"}))
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

  ==== __Example__

  >>> data ID = ID {unID :: Int} deriving Show
  >>> data User = User {userID :: ID, userName :: String} deriving Show
  >>> data UserError = UserError {ueID :: Maybe [String], ueName :: Maybe [String]} deriving Show

  >>> userValidator = UserError <$> check1 (unID . userID) (passIf ["invalid ID"] (>0)) <*> check1 userName (failIf ["username can't be empty"] null)

  >>> validUser01 = User (ID 11) "mastarija"
  >>> validUser02 = User (ID 13) "reygoch"

  >>> invalidUser01 = User (ID 0) ""
  >>> invalidUser02 = User (ID (-1)) "badboy"

  >>> validateP (checkN id userValidator) [validUser01, invalidUser01, validUser02, invalidUser02]
  Right (Just [Nothing,Just (UserError {ueID = Just ["invalid ID"], ueName = Just ["username can't be empty"]}),Nothing,Just (UserError {ueID = Just ["invalid ID"], ueName = Nothing})])
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

  ==== __Example__

  >>> newtype Database = Database { someData :: Int }
  >>> let check = \ i -> someData >>= \ d -> pure $ if d < i then Nothing else Just "'DB' data is greater than input"

  >>> validateM (make check) 5 (Database 14)
  Right "'DB' data is greater than input"

  >>> validateM (make check) 5 (Database 3)
  Left (Valid 5)
-}
validateM :: Monad m => Valor i m e -> i -> m ( Either ( Valid i ) e )
validateM ( Valor v ) i = v i >>= pure . wrong Right ( const $ Left $ Valid i )

