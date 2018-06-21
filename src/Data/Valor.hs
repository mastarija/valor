{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
module Data.Valor
  ( Validate
  , Validatable
  , Validator
  , skip
  , check
  , mapCheck
  , checks
  , mapChecks
  , subValidator
  , mapSubValidator
  , validate
  , validatePure
  , Identity (..)
  , ExceptT
  , throwE
  , runExceptT
  ) where
--
import Data.Maybe ( isJust )
import Data.Semigroup ( Semigroup, (<>) )

import Control.Applicative ( liftA2 )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE )

import Data.Functor.Identity ( Identity (..) )
--

--------------------------------------------------------------------------------
-- | A simple "tag" used to tell the 'Validatable' type family that we are
-- constructing the "error" type.
data Validate e


--------------------------------------------------------------------------------
-- | A simple type level function that is usefull to get rid of the boilerplate
-- in case you want your error and data type to have the same shape / structure.
type family Validatable a e x where
  Validatable Validate e x = Maybe e
  Validatable Identity e x = x
  Validatable a        e x = a x


--------------------------------------------------------------------------------
-- | Type that is used to carry the errors within 'Validator'. It's meant to be
-- used only internally.
data Validated e = Neutral | Valid e | Invalid e
  deriving ( Show )

instance Semigroup e => Semigroup ( Validated e ) where
  Neutral    <> x          = x
  x          <> Neutral    = x
  Valid   e1 <> Valid   e2 = Valid   $ e1 <> e2
  Valid   e1 <> Invalid e2 = Invalid $ e1 <> e2
  Invalid e1 <> Valid   e2 = Invalid $ e1 <> e2
  Invalid e1 <> Invalid e2 = Invalid $ e1 <> e2

instance Semigroup e => Monoid ( Validated e ) where
  mempty  = Neutral
  mappend = (<>)

instance Functor ( Validated ) where
  fmap _ Neutral     = Neutral
  fmap f (Valid e)   = Valid   $ f e
  fmap f (Invalid e) = Invalid $ f e

instance Applicative ( Validated ) where
  pure                     = Valid
  Neutral    <*> _         = Neutral
  _          <*> Neutral   = Neutral
  Valid   fe <*> Invalid e = Invalid $ fe e
  Valid   fe <*> Valid   e = Valid   $ fe e
  Invalid fe <*> Valid   e = Invalid $ fe e
  Invalid fe <*> Invalid e = Invalid $ fe e

--

--------------------------------------------------------------------------------
-- | With defined data types we can start constructing our 'Validator'. This can
-- be achieved by using 'skip', 'check', 'mapCheck', 'checks', 'mapChecks',
-- 'subValidator' and 'mapValidator', but before that we need to define some
-- "tests".
--
-- Tests should be in the form of @x -> ExceptT e m x@. 'ExceptT' was chosen for
-- the task because it allows us to use a custom monad and throw an error at the
-- same time.
--
-- __Pro tip:__ In case your test depends on the success or failure of another
-- field, you can use the 'State' monad or transformer to get the access to the
-- input data (you will have to repeat the validation of required field though).
--
-- With that in mind, let's define some basic tests:
--
-- > nonempty' :: Monad m => Text -> ExceptT String m Text
-- > nonempty' t = if Text.null t
-- >   then throwE "can't be empty"
-- >   else pure t
-- > 
-- > nonempty :: Monad m => Text -> ExceptT [String] m Text
-- > nonempty t = if Text.null t
-- >   then throwE ["can't be empty"]
-- >   else pure t
-- > 
-- > nonbollocks :: Monad m => Text -> ExceptT [String] m Text
-- > nonbollocks t = if t == "bollocks"
-- >   then throwE ["can't be bollocks"]
-- >   else pure t
-- > 
-- > nonshort :: Monad m => Text -> ExceptT [String] m Text
-- > nonshort t = if Text.length t < 10
-- >   then throwE ["too short"]
-- >   else pure t
--
newtype Validator i m e = Validator
  { unValidator :: i -> m (Validated e)
  }

instance ( Applicative m, Semigroup e ) => Semigroup ( Validator i m e ) where
  Validator x <> Validator y = Validator $ \i -> liftA2 (<>) (x i) (y i)

instance ( Applicative m, Semigroup e ) => Monoid ( Validator i m e ) where
  mempty  = Validator $ const (pure mempty)
  mappend = (<>)

instance Functor m => Functor ( Validator i m ) where
  fmap f (Validator v) = Validator $ \i -> fmap (fmap f) (v i)

instance Applicative m => Applicative ( Validator i m ) where
  pure x                      = Validator $ \_ -> pure $ pure x
  Validator x <*> Validator y = Validator $ \i -> (<*>) <$> x i <*> y i

--

--------------------------------------------------------------------------------
-- | This function is used to run the 'Validator' against the input data @i@,
-- once validation process is finished it will 'Maybe' return the error @e@
-- wrapped in the monad @m@ of your choice.
validate :: Functor m
  => Validator i m e -- ^ 'Validator' to run against the input data
  -> i               -- ^ input data that you want to validate
  -> m (Maybe e)     -- ^ result of the validation
validate (Validator v) i = fmap validatedtomaybe $ v i


--------------------------------------------------------------------------------
-- | In case you don't have a need for a monad you can use this function to run
-- your 'Validator' and get pure 'Maybe' instead of 'Maybe' wrapped in a monad.
validatePure ::
     Validator i Identity e -- ^ 'Validator' to run against the input data
  -> i                      -- ^ input data that you want to validate
  -> Maybe e                -- ^ result of the validation
validatePure v i = runIdentity $ validate v i

--

--------------------------------------------------------------------------------
-- | Use this in case you are not interested in validating a certain field.
skip :: Applicative m
  => Validator i m (Maybe e) -- ^ 'Validator' that never returns an error
skip = Validator $ \i -> pure $ Valid Nothing


--------------------------------------------------------------------------------
-- | Runs a single check against the specified field.
check :: forall i x m e. Monad m
  => (i -> x)                -- ^ field selector
  -> (x -> ExceptT e m x)    -- ^ field check
  -> Validator i m (Maybe e) -- ^ resulting 'Validator'
check sel chk = Validator $ \i -> validateprep <$> checkprep (chk $ sel i)


--------------------------------------------------------------------------------
-- | Runs a single check over every element of some 'Traversable' "container".
--
-- This is quite useful if you for example have a field that contains array of
-- items and you want to run a check against every single element of that list
-- instead of the list as a whole.
mapCheck :: forall i f x m e. ( Monad m, Traversable f )
  => (i -> f x)                          -- ^ field selector
  -> (x -> ExceptT e m x)                -- ^ field check
  -> Validator i m (Maybe (f (Maybe e))) -- ^ resulting 'Validator'
mapCheck sel chk = Validator $ \i -> do
  res <- mapM (checkprep . chk) (sel i)
  pure $ if any isJust res then Invalid $ Just res else Valid Nothing

--------------------------------------------------------------------------------
-- | Runs multiple checks against the specified field. Resulting error must be a
-- 'Semigroup' so that it can be combined or accumulated in some fashion,
-- most convenient thing would be to use a list of "something".
checks :: forall i x m e. ( Monad m, Semigroup e )
  => (i -> x)                -- ^ field selector
  -> [x -> ExceptT e m x]    -- ^ list of field checks
  -> Validator i m (Maybe e) -- ^ resulting 'Validator'
checks sel chks = Validator $ \i -> mconcat <$> mapM (mprep . ($ sel i)) chks

--------------------------------------------------------------------------------
-- | Basically the same thing as 'mapCheck' but it allows you to run multiple
-- checks per element.
mapChecks :: forall i f x m e. ( Monad m, Traversable f, Monoid e )
  => (i -> f x)                          -- ^ field selector
  -> [x -> ExceptT e m x]                -- ^ list of field checks
  -> Validator i m (Maybe (f (Maybe e))) -- ^ resulting 'Validator'
mapChecks sel chks = Validator $ \i -> do
  res <- mapM (helper chks) (sel i)
  pure $ if any isJust res then Invalid $ Just res else Valid Nothing
  where
    helper :: [x -> ExceptT e m x] -> x -> m (Maybe e)
    helper chks x = mconcat <$> mapM checkprep (fmap ($x) chks)


--------------------------------------------------------------------------------
-- | Runs a custom made 'Validator' against the field data.
subValidator :: forall i x m e. Functor m
  => (i -> x)                -- ^ field selector
  -> Validator x m e         -- ^ custom field 'Validator'
  -> Validator i m (Maybe e) -- ^ resulting 'Validator'
subValidator sel val = Validator $ \i ->
  validateprep <$> validate val (sel i)

--------------------------------------------------------------------------------
-- | Runs a custom made 'Validator' against the every element in a
-- 'Traversable' container.
mapSubValidator :: forall i f x m e. (Monad m, Traversable f)
  => (i -> f x)                          -- ^ field selector
  -> Validator x m e                     -- ^ custom field 'Validator'
  -> Validator i m (Maybe (f (Maybe e))) -- ^ resulting 'Validator'
mapSubValidator sel val = Validator $ \i -> do
  res <- mapM (validate val) (sel i)
  pure $ if any isJust res then Invalid $ Just res else Valid Nothing

--

mprep :: Monad m => ExceptT e m x -> m (Validated (Maybe e))
mprep = fmap validateprep . checkprep

checkprep :: Monad m => ExceptT e m x -> m (Maybe e)
checkprep = fmap (either Just (const Nothing)) . runExceptT

validateprep :: Maybe e -> Validated (Maybe e)
validateprep (Just e) = Invalid $ Just e
validateprep Nothing  = Valid   $ Nothing

validatedtomaybe :: Validated e -> Maybe e
validatedtomaybe Neutral     = Nothing
validatedtomaybe (Valid e)   = Nothing
validatedtomaybe (Invalid e) = Just e

--------------------------------------------------------------------------------
{- $introduction

__Valor__ strives to be a simple, intuitive and easy to use validation library.

It was inspired by libraries like [forma](https://hackage.haskell.org/package/forma)
and [digestive-functors](https://hackage.haskell.org/package/digestive-functors)
and some of their shortcomings.

For starters, both of those libraries are a bit too complicated (especially
digestive-functors) and restrictive about the data they work with and Valor
strives to remedy that. Another big problem is that they are essentially
libraries for writing parsers instead of validating data. The main difference
between Valor and existing validation libraries is that instead of trying to
parse the data from some fixed structure like JSON, it is trying to parse error
from the data (which can also be a JSON structure, Valor doesn't care).

__Anyway, try to read this documentation as a continuous tutorial since it__
__will explain all the steps necessary to validate some data.__
-}