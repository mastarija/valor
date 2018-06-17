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
  to get familiar with.

+ They are essentially parsers, and I personally don't like to manually handle 
  conversion of JSON fields from e.g. string to integers and other data types.

  Sure, it might be usefull to tell the user that he entered text instead of a
  number, but in that case I'd argue that your submission form is bad.

  Even in this case, it should still be possible to validate plain JSON with 
  Valor, but if that is your usecase I'd recommend you use [forma][1] for that
  since it was speciffically designed with JSON in mind.

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
  validation allways has to happen in the `Handler` at which point it is no
  longer in the JSON form and library like [forma][1] is not of much use to us
  unless we convert `SomeData` to JSON and parse it once again.

## Tutorial

To use Valor you obviously need to have a data types that you want to validate,
so let's define some:

```haskell
data Article = Article
  { id      :: Maybe Int
  , title   :: String
  , content :: String
  , tags    :: [String]
  , authors :: Maybe [User]
  } deriving ( Show )

data User = User
  { username :: String
  } deriving ( Show )
```

We also need to define some data types for our errors. They can look any way you
want but for now let's make errors have the same general structure as the data
that we are validating (you should enable `DuplicateRecordField` extension to
allow for the same field names in different records).

```haskell
data ArticleError = ArticleError
  { id      :: Maybe String
  , title   :: Maybe [String]
  , content :: Maybe [String]
  , tags    :: Maybe [Maybe String]
  , authors :: Maybe [Maybe UserError]
  } deriving ( Show )

data UserError = UserError
  { username :: Maybe [String]
  } deriving ( Show )
```

Important thing to note is that for errors every field is wrapped in `Maybe`
which doesn't contain the value but an error, or nothing if there is no error.

Error can be anything you want (or that has some meaning to you). If you want to
return multiple errors (or run multiple validations) for a single field, than
make sure your error is something that can be combined in some way, like a list
of strings.

Errors within "iterable" structures should also be wrapped in `Maybe` to
preserver the structure. If we are validating every element in a list, we don't
want to get a shorter list back if there are no errors for some elements.

Errors don't have to be just strings or a list of strings, you can define your
own data type that will for example hold the invalid value:

```haskell
data MyError a
  = TooShort a
  | NotEnoughNumbers a
  | NonExistingUser a
```

And than you can apply strategies for automatically correcting invalid data if
you are processing some database or something.

---

It might seem like defining `Article`, `ArticleError`, `User` and `UserError`
will lead to a lot of duplicated code, and you are right! It is a lot of
duplicated code but fear not, there is a solution.

If you are comfortable with using type families you can "fix" this by using
`Validatable` type family that comes with Valor. With it, instead of writing
everything twice, we can just write the following:

```haskell
data Article' a = Article
  { id      :: Validatable a String            Int
  , title   :: Validatable a [String]          String
  , content :: Validatable a [String]          String
  , tags    :: Validatable a [Maybe String]    [String]
  , authors :: Validatable a [Maybe (User' a)] (User' a)
  }

data User' a = User
  { username :: Validatable a [String] String
  }
```

`Validatable` is basically a type level function that takes types as arguments
and returns a type as a result.

In this case `Validatable` takes in 3 arguments.

+ First argument is a "function", or basically a type with a kind `* -> *` and 
  it mostly just serves as a tag which will tell `Validatable` what to
  construct the error or a value type.

+ Second argument is the error type (which will be wrapped in `Maybe` as a
  result).

+ Third argument is the actual value type that our "regular" data type will
  have.

Here is the `Validatable` definition:

```haskell
type family Validatable a e x where
  Validatable Validate e x = Maybe e
  Validatable Identity e x = x
  Validatable a        e x = a x
```

As you can see, there are two main cases. If we give `Identity` as an argument
to `Validatable` than we will get back just our regular value type, but if we
apply `Validate` than we get back the error type.

In case you pass in some other type with kind of `* -> *` than it will just get
applied to the regular value type.

---

> With all that in mind, remember that you don't have to use `Validatable` and
> you are free to define your own type family in case you have some other
> usecase.

---

Back to the topic at hand now. Once we have defined our core data types we have
to make our basic value and error types. Here is how we do that:

```haskell
type Article = Article' Identity
deriving instance Show Article

type ArticleError = Article' Validate
deriving instance Show ArticleError

type User = User' Identity
deriving instance Show User

type UserError = User' Validate
deriving instance Show UserError
```

Make sure you enable following language extensions so that you can derive `Show`
and other instances.

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
```


[1]: https://hackage.haskell.org/package/forma
[2]: https://hackage.haskell.org/package/digestive-functors
[3]: https://hackage.haskell.org/package/servant
[4]: https://hackage.haskell.org/package/aeson-1.4.0.0/docs/Data-Aeson.html#t:Value
