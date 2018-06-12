<p align="center">![Valor Logo](/valor-logo.svg "Valor")</p>

# Valor

Simple, minimal and easy to use Haskell validation library that gives you the
full control over how you handle your data types.

## Motivation

There are a few validation libraries out there, most notably [forma][1]
and [digestive-functors][3]. The problem I have with those is that they are all
either too speciffic or too complicated for my taste.

`digestive-functor`s use custom types to validate form and they even try to
handle the view generation. They are also more suited for `HTML` form validation
and rendering.

`forma` on the other hand doesn't try to do the rendering for me but it plays a
role of a `JSON` parser and forces me to use [Value][3] as the type of data that
I want to validate.

This might be acceptable if you are just doing some web development but if you
are doing some analysis and want to find out how much of the data fits your
requirements and than reformat it based on resulting errors than it can become
annoying to marshal you data to and from `JSON` format.

I'm also not satisfied how this looks in combination with [servant][2]. Usually
we would do something like this:

```
"user" :> "login" :> ReqBody '[JSON] UserCredentials :> Post '[JSON] Token
```

This way servant will receive `JSON` request and try to parse it to
`UserCredentials` automatically and throw an error if he doesn't succeed. After
that, we get `UserCredentials` into our `Handler` which we can than validate how
ever we please.

If we use `forma` for example, than we loose semantics that tell us what exactly
is being posted to the server e.g.:

```
"user" :> "login" :> ReqBody '[JSON] Value :> Post '[JSON] Token
```

Sure, it might be useful to tell the user if he entered a text instead of a
number, but than I'd argue that your form is broken if such mistake is possible,
or if we are talking about API than sent data is invalid and should result in
an error anyway.

My opinion is that we should let parsers do the parsing and validation libraries
do the validation.

With that said, it is still possible to send JSON `Value` to the server and
perform validation on it because Valor is agnostic about what kind of data you
put in and expect to get out.

[1]: https://hackage.haskell.org/package/forma
[2]: https://hackage.haskell.org/package/servant
[3]: https://hackage.haskell.org/package/digestive-functors
[4]: https://hackage.haskell.org/package/aeson-1.4.0.0/docs/Data-Aeson.html#t:Value
