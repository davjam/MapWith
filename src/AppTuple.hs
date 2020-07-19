{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module AppTuple
  (
  -- * Applying Multiple Arguments
    AppTuple(..)
    
  -- ** Argument Stacking
  -- $StackingFunctions
  , App1, App2, App3, App4
  , app1, app2, app3, app4
  
  -- ** Custom Argument Application
  -- $CustomArgApp
  )
where

{- ^
'AppTuple' represents the ability to apply any (fixed) number of arguments (embedded in 'args') to a function that returns a result of type 'b'.
The arguments can all be of different types.

For the instances provided here, the arguments have to be packaged into a "stacked tuple", so 'args' are (recursively) either:

- @()@: which represents zero arguments; or
- @(value, 'args')@: which represents the argument 'value' followed by the arguments in 'args'.

For example @(\'x\', (12 :: Int, (True, ())))@ represents a set of three arguments of different types:

- @\'x\' :: Char@;
- @12 :: Int@; and
- @True :: Bool@.
-}

class AppTuple args b where
  {- |
    The type of the function that can have 'args' types applied and that returns a result of type 'b'.
    For example:

    >>> egType :: FnType (Char, (Int, (Bool, ()))) String; egType = undefined
    >>> :t egType
    egType :: Char -> Int -> Bool -> [Char]
  -}
  type FnType args b :: *

  {- |
    Applies each argument in 'args' as a separate parameter to a function (of type @FnType args b@), and returns a 'b'.

    For example:

    >>> fn1 c n b = c : show n ++ if b then "hello" else "goodbye"
    >>> fn1 $# ('x', (12, (True, ())))
    "x12hello"
  -}
  ($#) :: FnType args b -> args -> b

instance AppTuple () b where
  type FnType () b = b
  f $# () = f

instance AppTuple t2 b => AppTuple (t1, t2) b where
  type FnType (t1, t2) b = t1 -> (FnType t2 b)
  f $# (t1, t2) = f t1 $# t2

{- $StackingFunctions
These types and functions make code look a little less weird. For example, you can write:

>>> fn1 $# app3 'x' 12 True

instead of

>>> fn1 $# ('x', (12, (True, ())))

Although these are only provided here for 1 to 4 arguments, you can use the "stacked tuple" to apply any number of arguments.
-}

type App1       a =            (a, ())
-- ^ A "stacked tuple" of one values
type App2     a b =        (a, (b, ()))
-- ^ A "stacked tuple" of two values
type App3   a b c =    (a, (b, (c, ())))
-- ^ A "stacked tuple" of three values
type App4 a b c d = (a,(b, (c, (d, ()))))
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

instance AppTuple MyStuff r where
  type FnType MyStuff r = Char -> Int -> Bool -> r
  f $# MyStuff c n b = f c n b
@

then:

>>> fn1 c n b = c : show n ++ if b then "hello" else "goodbye"
>>> fn1 $# MyStuff 'y' 7 False
"y7goodbye"
-}
