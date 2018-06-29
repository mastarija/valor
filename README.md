![Valor Build Status](https://api.travis-ci.org/reygoch/valor.svg?branch=master)
[![Valor Documentation](https://img.shields.io/badge/hackage-0.1.0.0-blue.svg)](https://hackage.haskell.org/package/valor-0.1.0.0)

# Valor

General, simple and easy to use structured validation library that gives you
control over the error and input types.

## Motivation

Currently there are a few validation libraries out there, most notably
[forma][1] and [digestive-functors][2]. They are acceptable but they are mostly
geard towards the web. Even with that in mind, I find them a bit impractical to
use so I've decided to make a validation library of my own.

In no particular order here are the main problems I have with them:

+ They limit what you can validate and what you get as a result of that
  validation. [Forma][1] expects a JSON [Value][4] as an input and gives you either parsed data as a result or an error in the form of a JSON [Value][4].

  Additionally, [digestive-functors][2] use a custom result type that you need
  to get familiar with to some extent.

+ They are essentially parsers, and I personally don't like to manually handle 
  conversion of JSON fields from e.g. string to integers and other data types.

  Sure, it might be useful to tell the user that he entered text instead of a
  number, but in that case I'd argue that your submission form is bad.

  Even in this case, it should still be possible to validate plain JSON with 
  Valor, but if that is your use case I'd recommend you use [forma][1] for that
  since it was specifically designed with JSON in mind.

+ They don't really play well with [servant][3]. Let's say that we have a record
  `SomeData`. If we wanted to allow users to submit that data to the server we'd
  have something like this :

  ```haskell
  "api" :> ReqBody '[JSON] SomeData :> Post '[JSON] SomeResponse
  ```

  User would send `SomeData` encoded as JSON to the server, [servant][3] would
  automagically parse it and pass it to the `Handler` for further processing.

  If we wanted to validate this data with let's say [forma][1] than we would
  have to write something like this:

  ```haskell
  "api" :> ReqBody '[JSON] Value :> Post '[JSON] SomeResponse
  ```

  in which case we lose nice semantics from the first example where it is
  obvious what data is being sent to the server (or at least what should've been
  sent).

  Since [servant][3] doesn't allow us to declare validation in the type,
  validation always has to happen in the `Handler` at which point it is no
  longer in the JSON form and library like [forma][1] is not of much use to us
  unless we convert `SomeData` to JSON and parse it once again.

---

## Tutorial

Before we get started, Valor uses `ExceptT` from the [transformers][5] package so make sure you add it as a dependency in your project.

First thing we usually want to do is to define our input data and error types.
We can define them separately by hand, or if our error and data types have the
same "shape" (same field names) we can use a handy type family to help us do
them all at once.

Lets declare our imports and required language extensions:

```haskell
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
--
module Tutorial where
--
import Prelude hiding ( id ) -- just so that we can use id as a field name
                             -- without any difficulties
import Data.Valor

import Data.Functor.Identity ( Identity (..) )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE )

import Data.Text ( Text )
import qualified Data.Text as Text
--
```

With that out of the way we can start defining our data types. As previously
stated, we can make both data and error types by hand like this (which would
additionally require the use of `DuplicateRecordField` extension):

```haskell
data User = User
  { username :: Text
  , password :: Text
  } deriving ( Show )

data UserError = UserError
  { username :: Maybe String   -- this field will have only one error
  , password :: Maybe [String] -- this one can have multiple errors
  } deriving ( Show )
```

This approach is perfectly valid and much more flexible since it allows you to
have different fields in data and error type, but if you want your types to
have the same field names than it might be easier to use `Validatable` type
family to get rid of the boilerplate and define them like this:

```haskell
data User' a = User
  { email    :: Validatable a String   Text
  , username :: Validatable a [String] Text
  }

type User = User' Identity
deriving instance Show User

type UserError = User' Validate
deriving instance Show UserError
```

This is equivalent to the first example, but much more maintainable. With this 
approach we have to use `StandaloneDeriving` and `TypeSynonymInstances`
language extensions to allow us instance derivation.

Right, let's define a more complex type now:

```haskell
data Article' a = Article
  { id      :: Validatable a String            Int
  , title   :: Validatable a [String]          Text
  , content :: Validatable a [String]          Text
  , tags1   :: Validatable a [Maybe [String]]  [Text] -- Here I want to have
                                                      -- multiple errors for a
                                                      -- single tag in a list.

  , tags2   :: Validatable a [Maybe String]    [Text] -- Here I want to have
                                                      -- only one reported
                                                      -- error per tag. 
  , author  :: Validatable a UserError         User
  , authors :: Validatable a [Maybe UserError] [User]
  }

type Article = Article' Identity
deriving instance Show Article

type ArticleError = Article' Validate
deriving instance Show ArticleError
```

`Validatable` type family is nothing smart. In fact, it is just a simple type
level function. Here is its definition, it should be obvious what it does:

```haskell
type family Validatable a e x where
  Validatable Validate e x = Maybe e
  Validatable Identity e x = x
  Validatable a        e x = a x
```

Ok, so now we have seen how `Validatable` type family works, we have defined 
data types that we want to validate and data types that will store our errors.
Before we start writing our validation rules (`Validator`s) we first need to
have some tests / checks to run against our field values so let's define some
simple ones:

```haskell
nonover18 :: Monad m => Int -> ExceptT String m Int
nonover18 n = if n < 18
  then throwE "must be greater than 18"
  else pure n

nonempty' :: Monad m => Text -> ExceptT String m Text
nonempty' t = if Text.null t
  then throwE "can't be empty"
  else pure t

nonempty :: Monad m => Text -> ExceptT [String] m Text
nonempty t = if Text.null t
  then throwE ["can't be empty"]
  else pure t

nonbollocks :: Monad m => Text -> ExceptT [String] m Text
nonbollocks t = if t == "bollocks"
  then throwE ["can't be bollocks"]
  else pure t

nonshort :: Monad m => Text -> ExceptT [String] m Text
nonshort t = if Text.length t < 10
  then throwE ["too short"]
  else pure t
```

Here the `ExceptT` transformer is used because it allows you to use your own
monad in case you need to access the database to validate some data. This is
also handy in case your test depends on the success or failure of some other
field value. In that case you can use the `State` monad or transformer to pass
in the full data being validated instead of just a current field value.

With that out of our way we can start writing our 'Validator's:

```haskell
userValidator :: Monad m => Validator User m UserError
userValidator = User
  <$> check  email nonempty'
  <*> checks username [nonempty, nonbollocks, nonshort]

articleValidator :: Monad m => Validator Article m ArticleError
articleValidator = Article
  <$> check           id      nonover18
  <*> checks          title   [nonempty, nonbollocks]
  <*> checks          content [nonempty, nonshort, nonbollocks]
  <*> mapChecks       tags1   [nonempty, nonbollocks]
  <*> mapCheck        tags2   nonempty'
  <*> subValidator    author  userValidator
  <*> mapSubValidator authors userValidator
```

As you can see, it is very simple and readable code. You just state what field
you want to validate and what tests you want to run against it. As a result you
get your error type (once you've ran your `Validator` against some actual data)
.

Let's define some sample data to test our `Validator` on:

```haskell
goodUser :: User
goodUser = User "hello@kitty.com" "kittyusername"

badUser :: User
badUser = User "boaty@mcboatface.com" "bollocks"

badArticle :: Article
badArticle = Article
  { id      = 17
  , title   = ""
  , content = "bollocks"
  , tags1   = ["", "tag01", "tag02"]
  , tags2   = ["tag01", ""]
  , author  = badUser
  , authors = [badUser, goodUser]
  }
```

And now we can run our `Article` `Validator` against some actual data and here
is how it's done:

```haskell
>>> validatePure articleValidator badArticle
Just
  ( Article
      { id = Just "must be greater than 18"
      , title = Just ["can't be empty"]
      , content = Just ["too short","can't be bollocks"]
      , tags1 = Just [Just ["can't be empty"],Nothing,Nothing]
      , tags2 = Just [Nothing,Just "can't be empty"]
      , author = Just
          ( User
              { email = Nothing
              , username = Just ["can't be bollocks","too short"]
              }
          )
      , authors = Just
        [ Just
            ( User
                { email = Nothing
                , username = Just ["can't be bollocks","too short"]
                }
            )
        ,Nothing
        ]
      }
  )
```

That's all folks! To learn more read the actual documentation.

[1]: https://hackage.haskell.org/package/forma
[2]: https://hackage.haskell.org/package/digestive-functors
[3]: https://hackage.haskell.org/package/servant
[4]: https://hackage.haskell.org/package/aeson-1.4.0.0/docs/Data-Aeson.html#t:Value
