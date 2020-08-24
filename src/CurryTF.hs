{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : CurryTF
Description : Provides curry/uncurry-like function for any number of parameters
Copyright   : (c) David James, 2020
License     : BSD3
Stability   : Experimental

A generalisation of 'curry' and 'uncurry' , allowing currying of any number of arguments of different types.


For the class instances provided here, the arguments are packaged into a "stacked tuple".
For example @(\'x\', (3 :: Int, (True, ())))@ represents a set of three arguments of different types:

- @\'x\' :: Char@;
- @3 :: Int@; and
- @True :: Bool@.

The TF stands for Type Family. I've given this the (possibly weird) name to avoid any conflict with similar implementations.
-}

module CurryTF
  (
  -- * Class
    CurryTF(..)
  , ($#)

  -- * Stacking Helpers
  -- $StackingFunctions
  , App1, App2, App3, App4
  , app1, app2, app3, app4
  
  -- * Custom CurryTF Implementations
  -- $CustomArgApp
  
  -- * Other Implementations
  -- $SeeAlso
  )
where

{- |
Given:

- a type 'args' containing n embedded arguments; and 
- a result type 'r'

@CurryTF args r@ represents the ability to convert either way between functions:

- @fCurried :: /each/ -> /argument/ -> /as/ -> /a/ -> /separate/ -> /parameter/ -> r@; and
- @fUncurried :: /all-arguments-embedded-in-a-single-parameter/ -> r@.

so that:

- @fCurried = curryN fUncurried@; and
- @fUncurried = uncurryN fCurried@.
-}

class CurryTF args r where
  {- |
    The type of the (curried) function that can have arguments of the types embedded in 'args' applied and that returns a result of type 'r'.
    For example:

    >>> :kind! FnType (Char, (Int, (Bool, ()))) String
    FnType (Char, (Int, (Bool, ()))) String :: *
    = Char -> Int -> Bool -> [Char]
  -}
  type FnType args r :: *

  {- |
    Embeds a number of separate arguments into a single 'args' parameter, applies 'args' to a function, and returns the result.

    For example:

    >>> fn1 (c, (n, (b, ()))) = c : replicate n '1' ++ if b then "hello" else "goodbye"
    >>> curryN fn1 'x' 3 True
    "x111hello"
    
    This also support partial application:
    
    >>> :t curryN fn1 'x'
    curryN fn1 'x' :: Int -> Bool -> [Char]
  -}

  curryN :: (args -> r) -> FnType args r

  {- |
    Applies each argument embedded in 'args' as a separate parameter to a function, and returns the result.

    For example:

    >>> fn2 c n b = c : replicate n '2' ++ if b then "hello" else "goodbye"
    >>> uncurryN fn2 ('x', (3, (True, ())))
    "x222hello"
  -}
  uncurryN :: FnType args r -> args -> r

-- | the application of zero arguments, giving @r@
instance CurryTF () r where
  type FnType () r = r
  curryN f = f ()
  uncurryN f () = f

-- | the application of @arg@, followed by the application of @moreArgs@ (recursively), giving @r@
instance CurryTF moreArgs r => CurryTF (arg, moreArgs) r where
  type FnType (arg, moreArgs) r = arg -> FnType moreArgs r
  curryN f a = curryN (\t -> f (a, t))
  uncurryN f (arg, moreArgs) = uncurryN (f arg) moreArgs

-- | A binary operator for 'uncurryN', so if values a, b and c are embedded in @args@ then @f $# args = f a b c@
($#) :: CurryTF args r => FnType args r -> args -> r
f $# args = uncurryN f args

{- $StackingFunctions
These types and functions can make code that uses the "stacked tupples" look a little less weird. For example, you can write:

>>> fn2 $# app3 'x' 3 True

instead of

>>> fn2 $# ('x', (3, (True, ())))

Although these are only provided here for 1 to 4 arguments, you can use the "stacked tuple" to apply any number of arguments.
-}
{-# INLINABLE app1 #-}
{-# INLINABLE app2 #-}
{-# INLINABLE app3 #-}
{-# INLINABLE app4 #-}

type App1       a =             (a, ())
-- ^ A "stacked tuple" of one value
type App2     a b =         (a, (b, ()))
-- ^ A "stacked tuple" of two values
type App3   a b c =     (a, (b, (c, ())))
-- ^ A "stacked tuple" of three values
type App4 a b c d = (a, (b, (c, (d, ()))))
-- ^ A "stacked tuple" of four values

app1 ::                a -> App1       a
-- ^ stacks one value
app2 ::           a -> b -> App2     a b
-- ^ stacks two values
app3 ::      a -> b -> c -> App3   a b c
-- ^ stacks three values
app4 :: a -> b -> c -> d -> App4 a b c d
-- ^ stacks four values

app1       a =             (a, ())
app2     a b =         (a, (b, ()))
app3   a b c =     (a, (b, (c, ())))
app4 a b c d = (a, (b, (c, (d, ()))))

{- $CustomArgApp
It is possible to define instances for other types, for example:

@
data MyStuff = MyStuff Char Int Bool

instance CurryTF MyStuff r where
  type FnType MyStuff r = Char -> Int -> Bool -> r
  curryN f c n b = f (MyStuff c n b)
  uncurryN f (MyStuff c n b) = f c n b
@

then:

>>> fn2 $# MyStuff 'y' 5 False
"y22222goodbye"
>>> fn3 (MyStuff c n b) = c : show n ++ show b
>>> curryN fn3 'p' 8 False
"p8False"

Doing this, especially for a type with multiple constructors, may not be sensible.
-}

{- $SeeAlso
There are similar implementations in:

1. [Data.Tuple.Curry](https://hackage.haskell.org/package/tuple/docs/Data-Tuple-Curry.html); and
1. [Data.Tuple.Ops](https://hackage.haskell.org/package/tuple-sop/docs/Data-Tuple-Ops.html).

These both take tuples of the form (arg1, arg2, .., argn), which is arguably easier to use.

I built this (instead of using those), for good and bad reasons including:

- I'm trying to improve my Haskell. TypeFamilies seemed to help here, so I got to start using those too.
- (1) has a limit of 32 args. OK that's probably enough, but it just seemed wrong to have any restriction.
- (2) Seems a little complex, and excesive for the needs here. (Though, from what I've read so far, the "stacked-tuples" here are in SOP form?). They also have a limit - in this case 10 args.
-}
