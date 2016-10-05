---
layout: post
title: Applicative style programming
tags: haskell, functional-programming
---

I want to explore the Applicative functor, more than a functor and less than
a monad.  The reason to use it is that, by being less powerful, it can be
applied in more situations.

This post is a companion to the talk I gave at [HaskellMAD][HaskellMAD], you can
find the slides of the [talk here][talk].

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Create your own Applicatives](#create-your-own-applicatives)
- [Free Applicatives](#free-applicatives)
- [Static Analysis](#static-analysis)
- [Conclusions](#conclusions)

<!-- markdown-toc end -->



Applicatives or _Idioms_, as they were named back then, were introduced in
2008, in a [Functional Pearl][functional pearls] named [_Idioms: applicative
programming with effects_][idioms].

As the begin of this writeup, let's first introduce the typeclass:

```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

Simple enough, this typeclass allows you to put elements inside an `Applicative`
context –with `pure`–, and _smash_ `Applicative` values together through `<*>`.

We can understand `<*>` as `fmap` from `Functor`, but with the function lifted
to an `Applicative` context.

But while you're scratching your head trying to imagine a moment in your
development career in which you have a function inside a container, let me show
you an example:

```haskell
Prelude> let square = \ x -> x * x
Prelude> let plusTwo = \ x -> x + 2
Prelude> let functions = [square, plusTwo]
Prelude> let numbers = [1,2,3]
Prelude> functions <*> numbers
[1,4,9,3,4,5]
```

As you can see, the `<*>` implementation for `Applicative [a]`, is the
application of each function in the first list to each element in the second
list.

But, you don't need to tell me that functions inside a list is not the most
common thing in the world.  Let's explore a more useful way of using
`Applicative`.


## Validation

Validation is a well known problem.  You have some data that you need to
validate before it makes part of your domain, easy.  For our validation
example we will use the `Either a b` type.  `Either` is a _sum type_, meaning
that the number of possible values of `Either a b` is the _sum_ of possible values
of type `a`, and possible values of type `b`.

Basically, the `a`s in our validations will be validation errors –represented as
strings– and the `b`s, the final values.

```haskell
data Person = Person {
  name :: String,
  lastName :: String
} deriving Show

data Err = String

validateName :: String -> Either Err String
validateName n = if n == "Pepe" then Right n
                                else Left "name is not Pepe"

validateLastName :: String -> Either Err String
validateLastName l = if l == "García" then Right l
                                      else Left "last name is not García"
```

Our `validateName` function takes a string as paramenter, and validates it as
a name, and the same does `validateLastName`.

Now, let'b build up a `validatePerson` function from the validation of each one
of its parts:

```haskell
validatePersonM :: String -> String -> Either String Person
validatePersonM n l = do
    vName <- validateName n
    vLast <- validateLastName l
    return $ Person vName vLast
```

Since `Either a` is a monad, we can use all the monadic tricks in our hat to
compose it, for example this _do block_.  But, the cool part of `Either`,
is that it is an `Applicative`, so we can use all the `Applicative` machinery
to compose our program.

```haskell
validatePersonA :: String -> String -> Either String Person
validatePersonA n l = Person <$> validateName n <*> validateLastName l
```

How can the last function possibly work? Well, in case you missed it, `<$>` is
the infix version of `fmap`. and the typing of the body of the function is the
following:

```haskell
Prelude> :t Person
Person :: String -> String -> Person
Prelude> :t Person <$> validateName "pepe"
Person <$> validateName "pepe" :: Either String (String -> Person)
Prelude> :t Person <$> validateName "pepe" <*> validateLastName "Garcia"
Person <$> validateName "pepe" <*> validateLastName "Garcia" :: Either String Person
```

## Create your own Applicatives

You cand do this directly by providing an instance of the typeclass, like
follows:

```haskell
data Maybe a = Just a
             | Nothing


instance Applicative Maybe where
    pure = Just

    Just f  <*> m = fmap f m
    Nothing <*> _ = Nothing
```

This will enable ourselves to use our new `Maybe` data type as an `Applicative`.
But there is another case I want to cover here.  What can we do when what we
want is to provide an `Applicative` instance for an _ADT_ we have?  We can use
_Free Applicatives_.


## Free Applicatives

_Free Applicatives_ are an abstraction over Applicatives, and are basically
a lift from Applicative's typeclass operations to constructors of an _Algebraic
Data Type_.

You can find more information about free applicatives [here][free].  Basically,
the definition of _Free Applicative_ is:

```haskell
data Ap f a where
    Pure :: a -> Ap f a
    Ap   :: f a -> Ap f (a -> b) -> Ap f b

instance Functor (Ap f) where
  fmap f (Pure a)   = Pure (f a)
  fmap f (Ap x y)   = Ap x ((f .) <$> y)

instance Apply (Ap f) where
  Pure f <.> y = fmap f y
  Ap x y <.> z = Ap x (flip <$> y <.> z)

instance Applicative (Ap f) where
  pure = Pure
  Pure f <*> y = fmap f y
  Ap x y <*> z = Ap x (flip <$> y <*> z)
```

Now, we can provide an instance of Applicatve for every data type with kind
`* -> *` we have!

Let's create a small _ADT_ for all the operations in our blog.  It could be
something like follows:

```haskell
{-# LANGUAGE GADTs #-}

import Control.Applicative.Free
import Control.Applicative

data Author = Author {
    name :: String,
    lastName :: String
} deriving Show

data Post = Post {
    id :: Int,
    title :: String,
    content :: String,
    excerpt :: String
} deriving Show

data BlogF a where
    GetPost :: Id -> BlogF Post
    GetAuthor :: Id -> BlogF Author


type Blog a = Ap BlogF a
```

The most important part of this block is where we define our language, the `BlogF`
GADT, and where we lift it using a _Free Applicative_.

Also, we will need to add some operations to make our blog usable. Let's create
_smart constructors_ for `GetPost`, and `GetAuthor` operations, but lifted to
the `Blog` type.

```haskell
getPost :: Id -> Blog Post
getPost id = liftAp $ GetPost id

getAuthor :: Id -> Blog Author
getAuthor id = liftAp $ GetAuthor id
```

As you can imagine, `liftAp` is part of the `Control.Applicative.Free` library,
and it takes a value of the type `f a`, and lifts it to the type `Ap f a`.

Now let's build a program for rendering a page of our blog!

```haskell
data Page = Page {
    post :: Post,
    author :: Author
} deriving Show

getPage :: Id -> Id -> Blog Page
getPage postId authorId = Page <$> getPost postId
                                  <*> getAuthor authorId
```

Boom! Since there were no depencencies between our `getPost`, and `getAuthor`
operations, we can compose them _idiomatically_([1](#1)).

But, there is a part of our problem of _rendering a page_ that we are lacking,
it is [drawing the rest of the f*cking owl][owl].  Right now, we are not going to the
database to fetch our post and our author, we are just creating values of all
the operations in our program.  We are building an _Abstract Syntax Tree_.

What we need right now is to interpret the _AST_ to an actual value of `Page`,
and that's done via _Natural Transformations_.  Natural Transformations are a
way of transforming one `Functor` into another while mantaining the internal
structure. So, what we want to create is a function like this:

```haskell
naturalTransformation :: BlogF a -> IO a
```

Why do we want to interpret to `IO`?  Because `IO` is where side effects occur
in Haskell.  Fetching elements from our database will return `IO a` actions.
Now let's do the actual implementation of the `interpret` function:

```haskell
interpret :: BlogF a -> IO a
interpret (GetPost id)   = putStrLn ("getting post " ++ show id ++ " from DB")   *> pure $ Post id "this is the post" "content of the post" "excerpt"
interpret (GetAuthor id) = putStrLn ("getting author " ++ show id ++ " from DB") *> pure $ Author "Pepe" "García"
```

And finally, let's wire everything together:

```haskell
main :: IO ()
main = do
    page <- runAp interpret $ getPage 1 1
    print page

-- Output:
-- getting post 1 from DB
-- getting author 1 from DB
-- Page {post = Post {id = 1, title = "this is the post", content = "content of the post", excerpt = "excerpt"}, author = Author {name = "Pepe", lastName = "Garc\237a"}}
```

And this, simply, is our fully functioning([2](#2)) blog!

As you can imagine, `runAp` is part of the [free][free] library, and it takes
first a _Natural Transformation_ on any two functors, and then an `Ap f a`, and
interprets the _AST_.

The newly introduced _AST_ is just an _Abstract Syntax Trees_, and we mention it
because that's what we are producing with the calls to `getPost` and
`getAuthor`.  This means that what we were doing when calling `getPost`, `getAuthor`,
or `getPage` we are just creating small programs, no executing code.  The part of
the code execution is done by our interpreter.

## Static Analysis

One of the coolest things that we can do with Applicative Abstract Syntax Trees
is static analysis([3](#3)).  And this is because applicative programs, unlike monadic
programs, are not dependent on runtime values.  In other words, we can know more
about our program, without evaluating it.

So now, imagine that you want to limit the number of requests to the DB that
your `Blog` programs have to 10.  If you know that any expression `GetAuthor` or
`GetPost` is gonna be interpreted to a SQL sentence, you can just count the
number of these, and you're done.  How can we achieve this?

```haskell
instance Monoid Int where
    mempty = 0
    mappend = (+)

countInstructions :: BlogF a -> Int
countInstructions _ = 1

main :: IO ()
main = do
    putStrLn "NUMBER OF REQUESTS TO THE DB:"
    print instructions
    where instructions = runAp_ countInstructions page
          page = getPage 1 1

-- Output:
--
-- NUMBER OF REQUESTS TO THE DB:
-- 2
```

`runAp_` is a modified version of `runAp`, and as an interpreter it takes
a function like:

```haskell
fn :: Monoid b => f a -> b
```

And `mappend`s all the elements produced by it together


## Conclusions

We need to identify when to use Applicatives.  It's a bit tricky in the
beginning because we are not so used to it but as a few rules of thumb, I use
the following:

Use Applicatives if your monadic expressions do not depends ones on the others,
or program does not depend on runtime values. This last sentence means that you
don't need to evaluate functions with given arguments to keep evaluating your
_AST_.


----

<a name="1">1</a>. _Idiom_ is a synonym of _Applicative Functor_.

<a name="2">2</a>. Only if you understand someting that runs on the command line, and does not have access to the database as _fully functioning_.

<a name="3">3</a>. Static analysis, is a technique with which we can know stuff about a program
   without evaluating it.

[HaskellMAD]: http://www.meetup.com/Haskell-MAD
[talk]: http://es.slideshare.net/JosLuisGarcaHernndez/applicative-style-programming
[idioms]: http://strictlypositive.org/Idiom.pdf
[functional pearls]: https://wiki.haskell.org/Research_papers/Functional_pearls
[free]: https://github.com/ekmett/free
[owl]: http://i.imgur.com/RadSf.jpg
