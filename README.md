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

Here are the two main problems I have with them:

+ They limit what you can validate and what you get as a result of that
  validation. [Forma][1] expects a JSON `Value` as an input and gives you either
  parsed data as a result or an error in the form of a JSON `Value`.

  Additionally, [digestive-functors][2] use a custom result type that you need
  to get familiar with.

+ They are essentially parsers, and I personally don't like to manually handle 
  conversion of JSON fields from e.g. string to integers and other data types.

  Sure, it might be usefull to tell the user that he entered text instead of a
  number, but in that case I'd argue that your submission form is bad.

  Even in this case, it should still be possible to validate plain JSON with 
  Valor, but if that is your usecase I'd recommend you use [forma][1] for that
  since it was speciffically designed with JSON in mined.



[1]: https://hackage.haskell.org/package/forma
[2]: https://hackage.haskell.org/package/digestive-functors
[3]: https://hackage.haskell.org/package/servant
[4]: https://hackage.haskell.org/package/aeson-1.4.0.0/docs/Data-Aeson.html#t:Value
