{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module AppTuple
  (
  -- * Applying Multiple Arguments
    CurryN(..)
  , ($#)
    
  -- ** Argument Stacking
  -- $StackingFunctions
  , App1, App2, App3, App4
  , app1, app2, app3, app4
  
  -- ** Custom Argument Application
  -- $CustomArgApp
  )
where

{- ^
'CurryN' represents the ability to apply any (fixed) number of arguments (embedded in 'args') to a function that returns a result of type 'b'.
The arguments can all be of different types.

For the instances provided here, the arguments have to be packaged into a "stacked tuple", so 'args' are (recursively) either:

- @()@: which represents zero arguments; or
- @(value, 'args')@: which represents the argument 'value' followed by the arguments in 'args'.

For example @(\'x\', (12 :: Int, (True, ())))@ represents a set of three arguments of different types:

- @\'x\' :: Char@;
- @12 :: Int@; and
- @True :: Bool@.
-}

class CurryN args r where
  {- |
    The type of the function that can have 'args' types applied and that returns a result of type 'r'.
    For example:

    >>> egType :: FnType (Char, (Int, (Bool, ()))) String; egType = undefined
    >>> :t egType
    egType :: Char -> Int -> Bool -> [Char]
  -}
  type FnType args r :: *

  {- |
    Applies each argument in 'args' as a separate parameter to a function (of type @FnType args r@), and returns a 'r'.

    For example:

    >>> fn1 c n b = c : show n ++ if b then "hello" else "goodbye"
    >>> fn1 $# ('x', (12, (True, ())))
    "x12hello"
  -}
  uncurryN :: FnType args r -> args -> r
  curryN :: (args -> r) -> FnType args r

instance CurryN () r where
  type FnType () r = r
  uncurryN f () = f
  curryN f = f ()

instance CurryN moreArgs r => CurryN (arg, moreArgs) r where
  type FnType (arg, moreArgs) r = arg -> (FnType moreArgs r)
  uncurryN f (arg, moreArgs) = uncurryN (f arg) moreArgs
  curryN f a = curryN (\t -> f (a, t))

-- | A binary operator for 'uncurryN', so if @args@ contains values @a@, @b@ and @c@ then @f $# args = f a b c@
($#) :: CurryN args r => FnType args r -> args -> r
f $# args = (uncurryN f) args

{- $StackingFunctions
These types and functions make code look a little less weird. For example, you can write:

>>> fn1 $# app3 'x' 12 True

instead of

>>> fn1 $# ('x', (12, (True, ())))

Although these are only provided here for 1 to 4 arguments, you can use the "stacked tuple" to apply any number of arguments.
-}

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

instance CurryN MyStuff r where
  type FnType MyStuff r = Char -> Int -> Bool -> r
  f $# MyStuff c n b = f c n b
@

then:

>>> fn1 c n b = c : show n ++ if b then "hello" else "goodbye"
>>> fn1 $# MyStuff 'y' 7 False
"y7goodbye"
-}
