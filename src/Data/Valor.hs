{-|
Module      :  Valor
Copyright   :  © 2018 Luka Hadžiegrić
License     :  MIT
Maintainer  :  Luka Hadžiegrić <reygoch@gmail.com>
Stability   :  experimental
Portability :  portable

This module provides a general way for validating data. It was inspired by
@forma@ and @digestive-functors@ and some of their shortcomings.
In short, approach taken by the 'Valor' is to try and parse the error from the
data instead of data from some fixed structured format like the JSON.

Main feature of 'Valor' is that you are not forced to use specific input type
like JSON, or to use specific output type like 'digestive-functors' 'View'.
You can use what ever you like as an input and use custom error type as the
output (although it does have to follow a specific format).

To use 'Valor' you first need to have some "input" data type that you want to
validate and an "error" data type that will store validation errors of your
data. Although the "shapes" of your input and error data types can differ, in the
most common use case your input and error would be of the same shape.

Here is an example:

> data Article = Article
>   { id      :: Int
>   , title   :: String
>   , content :: String
>   , tags    :: [String]
>   , author  :: User
>   } deriving ( Show )
> 
> data ArticleError = ArticleError
>   { id      :: Maybe String           -- ^ here I've intended for 'id' to have only one error message
>   , title   :: Maybe [String]         -- ^ for 'title' field there might be many error messages
>   , content :: Maybe [String]
>   , tags    :: Maybe [Maybe [String]] -- ^ here every 'tag' can have multiple error messages (or none)
>   , author  :: Maybe UserError        -- ^ here we have a possible 'UserError' in case validation fails
>   } deriving ( Show )
> 
> --
> 
> data User = User
>   { username :: String
>   } deriving ( Show )
> 
> data UserError = UserError
>   { username :: Maybe [String]
>   } deriving ( Show )

You might think that this will introduce a lot of duplicated code, and you are
right! But there is a solution. If you do not need the flexibility of this first
approach, you can use provided 'Validatable' type family to ease the pain (or
even write your own type family, 'Valor' doesn't care).

So, here is how the above code would look if we were to use type families:

> {# LANGUAGE FlexibleInstances    #}
> {# LANGUAGE StandaloneDeriving   #}
> {# LANGUAGE TypeSynonymInstances #}
>
> --
>
> data Article' a = Article
>   { id      :: Validatable a String           Int
>   , title   :: Validatable a [String]         String
>   , content :: Validatable a [String]         String
>   , tags    :: Validatable a [Maybe [String]] [String]
>   , author  :: Validatable a (User' a)        (User' a)
>   }
> 
> type Article = Article' Identity
> deriving instance Show Article
> 
> type ArticleError = Article' Validate
> deriving instance Show ArticleError
>
> --
>
> data User' a = User
>   { username :: Validatable a [String] String
>   }
> 
> type User = User' Identity
> deriving instance Show User
> 
> type UserError = User' Validate
> deriving instance Show UserError

As you can see, we have to enable a couple of language extensions to allow us
type class derivation with this approach.

'Validatable' is basically a type level function that takes three arguments and
returns a type.

* First argument has kind @* -> *@ which means it is a type that takes another
  type as an argument to make a concrete type. One common example of this is
  'Maybe'. In this case however, we can pass in 'Identity' to @Article'@ to
  create our "value/input" type and 'Validate' to create our "error" type. If we
  pass in any other type it will just get applied to the third argument (which
  is basic field value of our input type).

* Second argument is the type we want to use for storing error(s). This will be
  the resulting type of 'Validatable' but wrapped in 'Maybe' if we apply
  'Validate'.

* Third argument is the basic value type for the field of our input type. This
  will be the resulting type in case we apply 'Identity'
-}
--
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
module Data.Valor
  ( -- * Constructing a 'Validator'
    Validator
  , skip
  , check
  , checks
  , mapCheck
  , mapChecks
  , subValidator
  , mapSubValidator
    -- * Validating the data
  , validate
  , validatePure
    -- * Utilities
  , Validate
  , Validatable
    -- * Convenient re-exports
  , Identity (..)
  , ExceptT
  , runExceptT
  , throwE
  ) where
--
import Data.Maybe ( isJust )
import Data.Semigroup ( Semigroup, (<>) )

import Control.Applicative ( liftA2 )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE )

import Data.Functor.Identity ( Identity (..) )
--

{- Constructing a 'Validator' -}

--------------------------------------------------------------------------------
{-|
Now that we have defined our input and error data types we can start
constructing a 'Validator' for our data. In essence validator is just a function
that takes in an input @i@ and returns an error @e@ wrapped in a monad @m@ if
your input was invalid.

'Validator' is an 'Applicative' and you can construct a new one by using
functions: 'skip', 'check', 'mapCheck', 'checks', 'mapChecks', 'subValidator'
and 'mapSubValidator'. Those functions have to be provided with actual checks to
perform, and we define a single check by using 'ExceptT', so let's create some
simple checks to perform on our data:

> over18 :: Monad m => Int -> ExceptT String m Int
> over18 n
>   | n < 18    = throwE "must be over 18"
>   | otherwise = pure n
> 
> nonempty :: Monad m => String -> ExceptT [String] m String
> nonempty s
>   | length s == 0 = throwE ["can't be empty"]
>   | otherwise     = pure s
> 
> nonbollocks :: Monad m => String -> ExceptT [String] m String
> nonbollocks s
>   | s == "bollocks" = throwE ["can't be bollocks"]
>   | otherwise       = pure s
> 
> nonshort :: Monad m => String -> ExceptT [String] m String
> nonshort s = if length s < 10 then throwE ["too short"] else pure s

With this we can finally create 'Validator's for our 'User' and 'Article' data
types:

> articleValidator :: Monad m => Validator Article m ArticleError
> articleValidator = Article
>   <$> check        id      over18
>   <*> checks       title   [nonempty, nonbollocks]
>   <*> checks       content [nonempty, nonbollocks, nonshort]
>   <*> mapChecks    tags    [nonempty, nonbollocks]
>   <*> subValidator author  userValidator
> 
> userValidator :: Monad m => Validator User m UserError
> userValidator = User
>   <$> checks username [nonempty, nonbollocks]

-}
newtype Validator i m e = Validator
  { unValidator :: i -> m (Validated e)
  }

instance (Applicative m, Semigroup e) => Semigroup (Validator i m e) where
  Validator x <> Validator y = Validator $ \i -> liftA2 (<>) (x i) (y i)

instance Functor m => Functor (Validator i m) where
  fmap f (Validator v) = Validator $ fmap (fmap f) . v

instance Applicative m => Applicative (Validator i m) where
  pure x = Validator $ \i -> pure $ pure x
  Validator x <*> Validator y = Validator $ \i ->
    (<*>) <$> x i <*> y i


--------------------------------------------------------------------------------
-- | 'skip' is used when you are not interested in validating certain fields.
skip :: Applicative m =>
  Validator i m (Maybe e) -- ^ just a dummy validator that always succeeds.
skip = Validator $ \_ -> pure $ Valid Nothing


--------------------------------------------------------------------------------
-- | Check if a single condition is satisfied.
check :: forall i x m e. (Functor m, Monoid e)
  => (i -> x)                -- ^ field selector
  -> (x -> ExceptT e m x)    -- ^ check to be performed
  -> Validator i m (Maybe e) -- ^ resulting validator
check sel chk = mconvert $ Validator $ unValidator (validator chk) . sel


--------------------------------------------------------------------------------
-- | Check if mutiple conditions are satisfied.
checks :: forall i x m e. ( Applicative m, Monoid e, Semigroup e )
  => (i -> x)                -- ^ field selector
  -> [x -> ExceptT e m x]    -- ^ list of checks
  -> Validator i m (Maybe e) -- ^ resulting validator
checks sel chks = foldr1 (<>) $ fmap (check sel) chks

--------------------------------------------------------------------------------
-- | Apply a single check to multiple values within 'Traversable' structure.
mapCheck :: forall i f x m e. (Traversable f, Monad m, Monoid e)
  => (i -> f x)                          -- ^ field selector
  -> (x -> ExceptT e m x)                -- ^ check to be performed
  -> Validator i m (Maybe (f (Maybe e))) -- ^ resulting validator
mapCheck sel chk = Validator $ \i -> do
  res <- mapM (validate $ validator chk) (sel i)
  pure $ if any isJust res then Invalid $ Just res else Valid Nothing

--------------------------------------------------------------------------------
-- | Apply a multiple checks to values within 'Traversable' structure.
mapChecks :: forall i f x m e.
  ( Monad m
  , Monoid e
  , Traversable f
  , Semigroup (f (Maybe e))
  )
  => (i -> f x)                          -- ^ field selector
  -> [x -> ExceptT e m x]                -- ^ list of checks
  -> Validator i m (Maybe (f (Maybe e))) -- ^ resulting validator
mapChecks sel chks = foldr1 (<>) $ fmap (mapCheck sel) chks


--------------------------------------------------------------------------------
-- | Apply a 'Validator' instead of check to the field. This is useful when
-- validating nested records.
subValidator :: forall i x m e. (Functor m)
  => (i -> x)                -- ^ field selector
  -> Validator x m e         -- ^ 'Validator' to run against field value
  -> Validator i m (Maybe e) -- ^ resulting 'Validator'
subValidator sel (Validator x) = mconvert $ Validator $ \i -> x $ sel i

mapSubValidator :: forall i f x m e. ( Monad m, Traversable f )
  => (i -> f x)                          -- ^ field selector
  -> Validator x m e                     -- ^ 'Validator' to run against values
  -> Validator i m (Maybe (f (Maybe e))) -- ^ resulting 'Validator'
mapSubValidator sel val = Validator $ \i -> do
  res <- mapM (validate val) (sel i)
  pure $ if any isJust res then Invalid $ Just res else Valid Nothing


{- Validating the data -}

--------------------------------------------------------------------------------
{-|
Once you have constructed your 'Validator' you can run it against your
input data. If there were no validation errors you will get 'Nothing' wrapped
in a monad of your choice as a result.

Here is the result of running 'articleValidator' against some bad data:

> badArticle :: Article
> badArticle = Article
>   { id      = 17
>   , title   = "Some interesting title"
>   , content = "bollocks"
>   , tags    = ["I'm ok", "me too", "bollocks"]
>   , author  = badUser
>   }
>
> badUser :: User
> badUser = User ""

> >>> validatePure articleValidator badArticle
> Just
>   ( Article
>     { id = Just "must be over 18"
>     , title = Nothing
>     , content = Just ["can't be bollocks","too short"]
>     , tags = Just [Nothing,Nothing,Just ["can't be bollocks"]]
>     , author = Just (User {username = Just ["can't be empty"]})
>     }
>   )
-}
validate :: ( Functor m )
  => Validator i m e -- ^ 'Validator' that we want to run against the value
  -> i               -- ^ value that is being validated
  -> m (Maybe e)     -- ^ final result wrapped in a monad of our choosing
validate (Validator v) i = helper <$> v i
  where
    helper :: Validated e -> Maybe e
    helper (Invalid e) = Just e
    helper _           = Nothing


--------------------------------------------------------------------------------
-- | This will run your 'Validator' as a pure computation returning simple
-- 'Maybe' instead of it being wrapped in some monad.
validatePure :: Validator i Identity e -> i -> Maybe e
validatePure v i = runIdentity $ validate v i


{- Utilities -}

--------------------------------------------------------------------------------
-- | A simple type family used for constructing your data structure.
type family Validatable a e x where
  Validatable Validate e x = Maybe e
  Validatable Identity e x = x
  Validatable a        e x = a x

--------------------------------------------------------------------------------
-- | Tag used with type family to tell the compiler that we are constructing the
-- "error" record.
data Validate a

--------------------------------------------------------------------------------
-- Utility function used for converting 'Validator' error from one type to
-- another (it's not very useful).
converter
  :: forall i m e e'. Functor m
  => (e -> e')        -- ^ function to apply in case of 'Valid'
  -> (e -> e')        -- ^ function to apply in case of 'Invalid'
  -> Validator i m e  -- ^ initial 'Validator'
  -> Validator i m e' -- ^ resulting 'Validator'
converter valid invalid (Validator v) = Validator $ \i ->
  fmap helper $ v i
  where
    helper :: Validated e -> Validated e'
    helper (Valid   e) = Valid   $ valid   e
    helper (Invalid e) = Invalid $ invalid e


--------------------------------------------------------------------------------
-- | Another simple utility function for converting 'Validator' error this time
-- into a 'Maybe'.
mconvert :: Functor m => Validator i m e -> Validator i m (Maybe e)
mconvert = converter (const Nothing) Just


--------------------------------------------------------------------------------
-- | Internal utility function for constructing a 'Validator' from 'ExceptT'.
validator :: (Functor m, Monoid e)
  => (i -> ExceptT e m x)
  -> Validator i m e
validator chk = Validator $ \i ->
  either Invalid (const $ Valid mempty) <$> (runExceptT $ chk i)


{- Internal stuff -}

--------------------------------------------------------------------------------
-- | Internal datatype used for handling users error structure.
data Validated e = Valid e | Invalid e
  deriving ( Show )

instance Semigroup e => Semigroup (Validated e) where
  Valid _    <> x          = x
  x          <> Valid _    = x
  Invalid e1 <> Invalid e2 = Invalid (e1 <> e2)

instance Functor Validated where
  fmap f (Valid   e) = Valid   $ f e
  fmap f (Invalid e) = Invalid $ f e

instance Applicative Validated where
  pure                     = Valid
  Valid   ef <*> Valid   e = Valid   $ ef e
  Valid   ef <*> Invalid e = Invalid $ ef e
  Invalid ef <*> Valid   e = Invalid $ ef e
  Invalid ef <*> Invalid e = Invalid $ ef e


