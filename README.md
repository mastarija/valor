![valor-build-status](https://api.travis-ci.org/reygoch/valor.svg?branch=master)

<p align="center">
	<img
		src="valor-logo.svg?sanitize=true"
		alt="Valor Logo"
		title="Valor"
		style="max-width:100%;"
	>
</p>

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

This module provides a general way for validating data. It was inspired by
`forma` and `digestive-functors` and some of their shortcomings.
In short, approach taken by the Valor is to try and parse the error from the
data instead of data from some fixed structured format like the JSON.

Main feature of Valor is that you are not forced to use specific input type
like JSON, or to use specific output type like `digestive-functors` `View`.
You can use what ever you like as an input and use custom error type as the
output (although it does have to follow a specific format).

To use Valor you first need to have some "input" data type that you want to
validate and an "error" data type that will store validation errors of your
data. Although the "shapes" of your input and error data types can differ, in
the most common use case your input and error would be of the same shape.

Here is an example:

```haskell
import Data.Valor
--
data Article = Article
  { id      :: Int
  , title   :: String
  , content :: String
  , tags    :: [String]
  , author  :: User
  } deriving ( Show )

data ArticleError = ArticleError
  { id      :: Maybe String           -- ^ here I've intended for 'id' to have only one error message
  , title   :: Maybe [String]         -- ^ for 'title' field there might be many error messages
  , content :: Maybe [String]
  , tags    :: Maybe [Maybe [String]] -- ^ here every 'tag' can have multiple error messages (or none)
  , author  :: Maybe UserError        -- ^ here we have a possible 'UserError' in case validation fails
  } deriving ( Show )

data User = User
  { username :: String
  } deriving ( Show )

data UserError = UserError
  { username :: Maybe [String]
  } deriving ( Show )
```

You might think that this will introduce a lot of duplicated code, and you are
right! But there is a solution. If you do not need the flexibility of this first
approach, you can use provided `Validatable` type family to ease the pain (or
even write your own type family, Valor doesn't care).

So, here is how the above code would look if we were to use type families:

```haskell
{# LANGUAGE FlexibleInstances    #}
{# LANGUAGE StandaloneDeriving   #}
{# LANGUAGE TypeSynonymInstances #}
--
import Data.Valor
--
data Article' a = Article
  { id      :: Validatable a String           Int
  , title   :: Validatable a [String]         String
  , content :: Validatable a [String]         String
  , tags    :: Validatable a [Maybe [String]] [String]
  , author  :: Validatable a (User' a)        (User' a)
  }

type Article = Article' Identity
deriving instance Show Article

type ArticleError = Article' Validate
deriving instance Show ArticleError

data User' a = User
  { username :: Validatable a [String] String
  }

type User = User' Identity
deriving instance Show User

type UserError = User' Validate
deriving instance Show UserError
```

As you can see, we have to enable a couple of language extensions to allow us
type class derivation with this approach.

`Validatable` is basically a type level function that takes three arguments and
returns a type.

+ First argument has kind @* -> *@ which means it is a type that takes another
  type as an argument to make a concrete type. One common example of this is
  `Maybe`. In this case however, we can pass in `Identity` to `Article'` to
  create our "value/input" type and 'Validate' to create our "error" type. If we
  pass in any other type it will just get applied to the third argument (which
  is basic field value of our input type).
+ Second argument is the type we want to use for storing error(s). This will be
  the resulting type of `Validatable` but wrapped in `Maybe` if we apply
  `Validate`.
+ Third argument is the basic value type for the field of our input type. This
  will be the resulting type in case we apply 'Identity'

[1]: https://hackage.haskell.org/package/forma
[2]: https://hackage.haskell.org/package/digestive-functors
[3]: https://hackage.haskell.org/package/servant
[4]: https://hackage.haskell.org/package/aeson-1.4.0.0/docs/Data-Aeson.html#t:Value
