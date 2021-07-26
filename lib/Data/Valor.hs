{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
--
module Data.Valor
  ( -- * Introduction
    -- $introduction

    -- * Defining data types
    Valor
  , Validatable

    -- * Creating a 'Validator'
  , Validator
  , skip
  , check
  , mapCheck
  , checks
  , mapChecks
  , subValidator
  , mapSubValidator

    -- * Validating data
  , validate
  , validatePure
  ) where
--
import Data.Maybe ( isJust )

import Control.Applicative ( liftA2 )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )

import Data.Functor.Identity ( Identity (..) )
--

class Check f v o where
  check' :: f -> v -> o

instance forall i x e m. Monad m
  => Check ( i -> x ) ( x -> ExceptT e m x ) ( Validator i m ( Maybe e ) ) where
  check' sel chk = Validator $ \ i -> validateprep <$> checkprep ( chk $ sel i )

instance forall i f x m e. ( Monad m , Traversable f )
  => Check ( i -> f x ) ( x -> ExceptT e m x ) ( Validator i m ( Maybe ( f ( Maybe e ) ) ) ) where
  check' sel chk = Validator $ \ i -> do
    res <- mapM ( checkprep . chk ) ( sel i )
    pure $ if any isJust res then Wrong $ Just res else Sound Nothing

instance forall i x m e. ( Monad m , Semigroup e )
  => Check ( i -> x ) [ x -> ExceptT e m x ] ( Validator i m ( Maybe e ) ) where
  check' sel chks = Validator $ \i -> mconcat <$> mapM (mprep . ($ sel i)) chks

instance forall i f x m e. ( Monad m , Traversable f , Monoid e )
  => Check ( i -> f x ) [ x -> ExceptT e m x ] ( Validator i m ( Maybe ( f ( Maybe e ) ) ) ) where
  check' sel chks = Validator $ \i -> do
    res <- mapM (helper chks) (sel i)
    pure $ if any isJust res then Wrong $ Just res else Sound Nothing
    where
      helper :: [x -> ExceptT e m x] -> x -> m (Maybe e)
      helper chks' x = mconcat <$> mapM checkprep (fmap ($x) chks')

instance forall i x m e. Functor m
  => Check ( i -> x ) ( Validator x m e ) ( Validator i m ( Maybe e ) ) where
  check' sel val = Validator $ \i ->
    validateprep <$> validate val (sel i)

instance forall i f x m e. ( Monad m , Traversable f )
  => Check ( i -> f x ) ( Validator x m e ) ( Validator i m ( Maybe ( f ( Maybe e ) ) ) ) where
  check' sel val = Validator $ \i -> do
    res <- mapM (validate val) (sel i)
    pure $ if any isJust res then Wrong $ Just res else Sound Nothing

{-

  I1 : a
  I2 : f a

  C1 : a -> c
  C2 : [ a -> c ]

  O1 : Maybe e
  O2 : Semigroup e => Maybe e
  O3 : Monoid e => Maybe ( f ( Maybe b ) )

  1 : ( i -> x ) -> ( x -> ExceptT e m x ) -> Validator i m ( Maybe e )
  2 : ( i -> f x ) -> ( x -> ExceptT e m x ) -> Validator i m ( Maybe ( f ( Maybe e ) ) )

  3 : ( i -> x ) -> [ x -> ExceptT e m x ] -> Validator i m ( Maybe e )
  4 : ( i -> f x ) -> [ x -> ExceptT e m x ] -> Validator i m ( Maybe ( f ( Maybe e ) ) )

  5 : ( i -> x ) -> Validator x m e -> Validator i m ( Maybe e )
  6 : ( i -> f x ) -> Validator x m e -> Validator i m ( Maybe f ( Maybe e ) )

  As an input to 'check' I can have an element, or many elements
  As a check, I can perform one check per element, or multiple checks per element
  As output
    If I have one element
      And if I have one check
        I get single error
      And if I have multiple check
        I get multiple errors
    If I have multiple elements
      And if I have one check
        I get list of errors
      And if I have multiple checks
        I get list of lists of errors

-}

--------------------------------------------------------------------------------
-- | Simple "tags" used in conjunction with the 'Validatable' type family.
data Valor
  = Simple
  | Report

--------------------------------------------------------------------------------
-- | A simple type level function that is usefull to get rid of the boilerplate
-- in case you want your error and data type to have the same shape / structure.
--
-- It takes in three arguments:
-- 
-- [@a@] A type with a kind of @* -> *@ that basically serves as a flag which
--    determines if 'Validatable' will return the error type wrapped in 'Maybe'
--    or a value type. To return the error type use 'Validate' and to return
--    value type use 'Identity'.
-- [@e@] Type that should be used for the error.
-- [@x@] Type that should be used for the value.
--
-- Here is an example of how to use it to reduce boilerplate, instead of this
-- (sill perfectly acceptable by Valor):
--
-- @
-- { -\# LANGUAGE DuplicateRecordFields \#- }
-- --
-- module Test
-- --
-- import Data.Text ( 'Data.Text.Text' )
-- --
-- data User = User
--   { username :: 'Data.Text.Text'
--   , password :: 'Data.Text.Text'
--   } deriving ( 'Show' )
--
-- data UserError = UserError
--   { username :: 'Maybe' 'String'   -- this field will have only one error
--   , password :: 'Maybe' ['String'] -- this one can have multiple errors
--   } deriving ( 'Show' )
-- @
--
-- which can get painful to maintain an repetitive to write if you have a lot
-- of fields in your records, you can just write the following:
--
-- @
-- { -\# LANGUAGE FlexibleInstances \#- }
-- { -\# LANGUAGE StandaloneDeriving \#- }
-- { -\# LANGUAGE TypeSynonymInstances \#- }
-- --
-- module Test
-- --
-- import Data.Valor ( 'Validatable', 'Validate' )
-- import Data.Text ( 'Data.Text.Text' )
-- import Data.Functor.Identity ( 'Identity' (..) )
-- --
-- data User' a = User
--   { username :: 'Validatable' a 'String'   'Data.Text.Text'
--   , password :: 'Validatable' a ['String'] 'Data.Text.Text'
--   }
-- 
-- type User = User' 'Identity'
-- deriving instance 'Show' User
-- 
-- type UserError = User' 'Validate'
-- deriving instance 'Show' UserError
-- @
--
-- This approach requires a few language extensions to allow us instance
-- derivation, but it removes a lot of the boilerplate and maintenance costs in
-- the long run.
--
-- All in all, 'Validatable' is quite easy to understand, it takes around 5 min
-- to understand this type family even if you've never used type families before
-- , just take a look at the __Equations__ below:
type family Validatable ( a :: Valor ) e x where
  Validatable 'Simple e x = x
  Validatable 'Report e x = Maybe e

--

--------------------------------------------------------------------------------
-- | Type that is used to carry the errors within 'Validator'. It's meant to be
-- used only internally.
data Validated e = Neutral | Sound e | Wrong e
  deriving ( Show )

instance Semigroup e => Semigroup ( Validated e ) where
  Neutral <> x       = x
  x       <> Neutral = x
  Sound a <> Sound b = Sound $ a <> b
  Sound a <> Wrong b = Wrong $ a <> b
  Wrong a <> Sound b = Wrong $ a <> b
  Wrong a <> Wrong b = Wrong $ a <> b

instance Semigroup e => Monoid ( Validated e ) where
  mempty  = Neutral
  mappend = (<>)

instance Functor Validated where
  fmap _ Neutral   = Neutral
  fmap f (Sound e) = Sound $ f e
  fmap f (Wrong e) = Wrong $ f e

instance Applicative Validated where
  pure                 = Sound
  Neutral  <*> _       = Neutral
  _        <*> Neutral = Neutral
  Sound fe <*> Wrong e = Wrong $ fe e
  Sound fe <*> Sound e = Sound $ fe e
  Wrong fe <*> Sound e = Wrong $ fe e
  Wrong fe <*> Wrong e = Wrong $ fe e

--

--------------------------------------------------------------------------------
-- | In case the value has been successfuly validated and contains no errors, it
-- will be returned wrapped in this @newtype@. Constructor for valid is not
-- exported / public, so the @'Valid' a@ can only be constructed through the
-- validation process, ensuring that you don't "accidentally" send invalid value
-- where it shouldn't go.
-- newtype Valid a = Valid
--   { unValid :: a
--   }

--------------------------------------------------------------------------------
-- | 'Validator' is basically a function that takes in an input @i@ and returns
-- an error @e@ wrapped in your monad of choice @m@.
--
-- To construct a 'Validator' you can use functions 'skip', 'check', 'mapCheck',
-- 'checks', 'mapChecks', 'subValidator' and 'mapSubValidator'. Intended way of
-- constructing a 'Validator' is by using the 'Applicative' interface.
--
-- Above mentioned functions expect a test (or tests) in the form of
-- @x -> ExceptT e m x@. 'ExceptT' was chosen here because it is a monad
-- transformer and allows ust to throw an error and use a custom monad @m@.
-- This is useful in case you have to check the database to validate some data
-- or your test relies on success or failure of another field. You can use state
-- monad or transformer to pass in the data being validated so that it is
-- accessible within the test.
--
-- To run your 'Validator' against some data you can use 'validate' function, or
-- 'validatePure' if you don't want to use any particular monad and get the pure
-- result wrapped in 'Maybe'.
--
-- Here is an example of a few simple tests and construction of a 'Validator'
-- for the previously defined @User@ record:
--
-- @
-- nonempty' :: 'Monad' m => 'Text.Text' -> 'ExceptT' 'String' m 'Text.Text'
-- nonempty' t = if 'Text.null' t
--   then 'throwE' "can't be empty"
--   else 'pure' t
--
-- nonempty :: 'Monad' m => 'Text.Text' -> 'ExceptT' ['String'] m 'Text.Text'
-- nonempty t = if 'Text.null' t
--   then 'throwE' ["can't be empty"]
--   else 'pure' t
-- 
-- nonbollocks :: 'Monad' m => 'Text.Text' -> 'ExceptT' ['String'] m 'Text.Text'
-- nonbollocks t = if t == "bollocks"
--   then 'throwE' ["can't be bollocks"]
--   else 'pure' t
-- 
-- nonshort :: 'Monad' m => 'Text.Text' -> 'ExceptT' ['String'] m 'Text.Text'
-- nonshort t = if 'Text.length' t < 10
--   then 'throwE' ["too short"]
--   else 'pure' t
-- @
--
-- @
-- userValidator :: 'Monad' m => 'Validator' User m UserError
-- userValidator = User
--   '<$>' 'check'  email nonempty'
--   '<*>' 'checks' username [nonempty, nonbollocks, nonshort]
-- @
--
newtype Validator i m e = Validator
  { unValidator :: i -> m (Validated e)
  }

instance ( Applicative m, Semigroup e ) => Semigroup ( Validator i m e ) where
  Validator x <> Validator y = Validator $ \i -> liftA2 (<>) (x i) (y i)

instance ( Applicative m, Semigroup e ) => Monoid ( Validator i m e ) where
  mempty  = Validator $ const $ pure mempty
  mappend = (<>)

instance Functor m => Functor ( Validator i m ) where
  fmap f (Validator v) = Validator $ \i -> fmap (fmap f) (v i)

instance Applicative m => Applicative ( Validator i m ) where
  pure x                      = Validator $ \_ -> pure $ pure x
  Validator x <*> Validator y = Validator $ \i -> (<*>) <$> x i <*> y i

--

--------------------------------------------------------------------------------
-- | Use this in case you are not interested in validating a certain field.
skip :: Applicative m
  => Validator i m (Maybe e) -- ^ 'Validator' that never returns an error
skip = Validator $ \ _ -> pure $ pure Nothing


--------------------------------------------------------------------------------
-- | Runs a single check against the specified field.
check :: forall i x m e. Monad m
  => (i -> x)                -- ^ field selector
  -> (x -> ExceptT e m x)    -- ^ field check
  -> Validator i m (Maybe e) -- ^ resulting 'Validator'
check = check'


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
mapCheck = check'


--------------------------------------------------------------------------------
-- | Runs multiple checks against the specified field. Resulting error must be a
-- 'Semigroup' so that it can be combined or accumulated in some fashion,
-- most convenient thing would be to use a list of "something".
checks :: forall i x m e. ( Monad m, Semigroup e )
  => (i -> x)                -- ^ field selector
  -> [x -> ExceptT e m x]    -- ^ list of field checks
  -> Validator i m (Maybe e) -- ^ resulting 'Validator'
checks = check'

--------------------------------------------------------------------------------
-- | Basically the same thing as 'mapCheck' but it allows you to run multiple
-- checks per element.
mapChecks :: forall i f x m e. ( Monad m, Traversable f, Monoid e )
  => (i -> f x)                          -- ^ field selector
  -> [x -> ExceptT e m x]                -- ^ list of field checks
  -> Validator i m (Maybe (f (Maybe e))) -- ^ resulting 'Validator'
mapChecks = check'

--------------------------------------------------------------------------------
-- | Runs a custom made 'Validator' against the field data.
subValidator :: forall i x m e. Functor m
  => (i -> x)                -- ^ field selector
  -> Validator x m e         -- ^ custom field 'Validator'
  -> Validator i m (Maybe e) -- ^ resulting 'Validator'
subValidator = check'


--------------------------------------------------------------------------------
-- | Runs a custom made 'Validator' against the every element in a
-- 'Traversable' container.
mapSubValidator :: forall i f x m e. (Monad m, Traversable f)
  => (i -> f x)                          -- ^ field selector
  -> Validator x m e                     -- ^ custom field 'Validator'
  -> Validator i m (Maybe (f (Maybe e))) -- ^ resulting 'Validator'
mapSubValidator = check'

--

--------------------------------------------------------------------------------
-- | This function is used to run the 'Validator' against the input data @i@,
-- once validation process is finished it will 'Maybe' return the error @e@
-- wrapped in the monad @m@ of your choice.
validate :: Functor m
  => Validator i m e -- ^ 'Validator' to run against the input data
  -> i               -- ^ input data that you want to validate
  -> m (Maybe e)     -- ^ result of the validation
validate v i = fmap validatedtomaybe $ ( unValidator v ) i


--------------------------------------------------------------------------------
-- | In case you don't have a need for a monad you can use this function to run
-- your 'Validator' and get pure 'Maybe' instead of 'Maybe' wrapped in a monad.
--
-- Here is an example of running @userValidator@ over some invalid data:
--
-- @
-- badUser :: User
-- badUser = User "boaty@mcboatface.com" "bollocks"
-- @
--
-- @
-- >>> 'validatePure' userValidator badUser
-- 'Just' (User {email = 'Nothing', username = 'Just' ["can't be bollocks","too short"]})
-- @
validatePure ::
     Validator i Identity e -- ^ 'Validator' to run against the input data
  -> i                      -- ^ input data that you want to validate
  -> Maybe e                -- ^ result of the validation
validatePure v i = runIdentity $ validate v i

--

mprep :: Monad m => ExceptT e m x -> m (Validated (Maybe e))
mprep = fmap validateprep . checkprep

checkprep :: Monad m => ExceptT e m x -> m (Maybe e)
checkprep = fmap (either Just (const Nothing)) . runExceptT

validateprep :: Maybe e -> Validated (Maybe e)
validateprep (Just e) = Wrong $ Just e
validateprep Nothing  = Sound $ Nothing

validatedtomaybe :: Validated e -> Maybe e
validatedtomaybe Neutral   = Nothing
validatedtomaybe (Sound _) = Nothing
validatedtomaybe (Wrong e) = Just e

--------------------------------------------------------------------------------
-- $introduction
-- 
-- __Valor__ strives to be a simple and easy to use data validation library,
-- that returns errors in a structured format. It is mainly focused on
-- validating records, but with enough imagination you can validate anything
-- (I think).
-- 
-- The usual approach to validating that most validation libraries (like
-- [digestive-functors](https://hackage.haskell.org/package/digestive-functors)
-- and [forma](https://hackage.haskell.org/package/forma)) take is to act as a
-- parser that tries to parse the input data into some output data and returns
-- an error if it doesn't succeed.
-- 
-- Valors approach is to leave the parsing to parsing libraries and instead try
-- to "parse" the __error__ from already parsed data instead of the data it
-- self. This approach is more transparent since it doesn't force you to use any
-- intermediate types like @JSON@ which expects you to know, or learn how to
-- work with them, instead you decide what goes in and what comes out and have
-- (almost) full control over your data types.