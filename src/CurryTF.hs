{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
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

For the 'HList' instance provided here, the arguments are packaged into a heterogeneous list.
For example @\'x\' :\# 3 :\# True :\# HNil :: HList '[Char, Int, Bool]@ represents a set of three arguments of different types:

- @\'x\' :: Char@;
- @3 :: Int@; and
- @True :: Bool@.

The TF stands for Type Family. I've given this the (possibly weird) name to avoid any conflict with similar implementations.
-}

module CurryTF
  (
  -- * CurryTF
    CurryTF(..)
  , ($#)
  
  -- * HList
  , HList(..)

  -- * Multiple-Argument Helpers
  -- $MultiArgHelpers
  , App1, App2, App3, App4
  , app1, app2, app3, app4
  
  -- * Custom CurryTF Implementations
  -- $CustomArgApp
  
  -- * Other Implementations
  -- $SeeAlso
  )
where

{- |
A basic implementation of heterogeneous lists that are an instance of CurryTF.
-}

data HList xs where
    HNil :: HList '[]
    (:#) :: a -> HList as -> HList (a ': as)

infixr 6 :#

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show a, Show (HList xs)) => Show (HList (a ': xs)) where
  show (a :# xs) = show a ++ " :# " ++ show xs

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

    >>> :kind! FnType (HList '[Char, Int, Bool]) String
    = Char -> Int -> Bool -> [Char]
  -}
  type FnType args r :: *

  {- |
    Converts an uncurried function of multiple arguments (embedded in a single parameter) into a curried function of those arguments (as separate parameters).

    For example:

    >>> fn1 :: HList '[Char, Int, Bool] -> String; fn1 (c :# n :# b :# HNil) = c : replicate n '1' ++ if b then "hello" else "goodbye"
    >>> :t curryN fn1
    curryN fn1 :: Char -> Int -> Bool -> [Char]
    >>> curryN fn1 'x' 3 True
    "x111hello"
  -}

  curryN :: (args -> r) -> FnType args r

  {- |
    Converts a curried function (that take multiple arguments, each as a separate parameter) into an uncurried function of those arguments (all embedded in a single parameter).

    For example, given:

    >>> fn2 c n b = c : replicate n '2' ++ if b then "hello" else "goodbye"
    
    @uncurryN fn2@ is ambiguous about how many arguments should be packaged together. This is resolved either by specifying a type of the single packed parameter or the result, for example:

    >>> uncurryN fn2 ('x' :# 3 :# True :# HNil)
    "x222hello"
    >>> uncurryN fn2 ('x' :# 3 :# HNil) True
    "x222hello"
    >>> :t uncurryN fn2 :: HList '[Char, Int, Bool] -> _
    Found type wildcard ‘_’ standing for ‘[Char]’
    >>> :t uncurryN fn2 :: HList '[Char, Int] -> _
    Found type wildcard ‘_’ standing for ‘Bool -> [Char]’
  -}
  uncurryN :: FnType args r -> args -> r

-- | the application of zero arguments, giving @r@
instance CurryTF (HList '[]) r where
  type FnType (HList '[]) r = r
  curryN f = f HNil
  uncurryN f HNil = f

-- | the application of @arg@, followed by the application of @moreArgs@ (recursively), giving @r@
instance CurryTF (HList moreArgs) r => CurryTF (HList (arg ': moreArgs)) r where
  type FnType (HList (arg ': moreArgs)) r = arg -> FnType (HList moreArgs) r
  curryN f arg = curryN (\moreArgs -> f (arg :# moreArgs))
  uncurryN f (arg :# moreArgs) = uncurryN (f arg) moreArgs

-- | A binary operator for 'uncurryN', so if values a, b and c are embedded in @args@ then @f $# args = f a b c@
($#) :: CurryTF args r => FnType args r -> args -> r
f $# args = uncurryN f args

infixr 0 $#

{- $MultiArgHelpers
These types and functions make it easier to define and use simple heterogeneous lists:

>>> fn2 $# app3 'x' 3 True

instead of

>>> fn2 $# 'x' :# 3 :# True :# HNil

These are only provided here for 1 to 4 arguments, but you can use the 'HList' directly to apply any number of arguments.
-}
{-# INLINABLE app1 #-}
{-# INLINABLE app2 #-}
{-# INLINABLE app3 #-}
{-# INLINABLE app4 #-}

type App1 a       = HList '[a]
-- ^ An application of one value
type App2 a b     = HList '[a,b]
-- ^ An application of two values
type App3 a b c   = HList '[a,b,c]
-- ^ An application of three values
type App4 a b c d = HList '[a,b,c,d]
-- ^ An application of four values

app1 :: a                -> App1 a
-- ^ applies one value
app2 :: a -> b           -> App2 a b
-- ^ applies two values
app3 :: a -> b -> c      -> App3 a b c
-- ^ applies three values
app4 :: a -> b -> c -> d -> App4 a b c d
-- ^ applies four values

app1 a       = a                :# HNil
app2 a b     = a :# b           :# HNil
app3 a b c   = a :# b :# c      :# HNil
app4 a b c d = a :# b :# c :# d :# HNil

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
