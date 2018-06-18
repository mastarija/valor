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

[1]: https://hackage.haskell.org/package/forma
[2]: https://hackage.haskell.org/package/digestive-functors
[3]: https://hackage.haskell.org/package/servant
[4]: https://hackage.haskell.org/package/aeson-1.4.0.0/docs/Data-Aeson.html#t:Value
