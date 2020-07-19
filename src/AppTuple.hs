{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module AppTuple
  (
    AppTuple(..)
  , App1, App2, App3, App4
  , app1, app2, app3, app4
  )
where

class AppTuple tupType b where
  type FnType tupType b :: *
  ($#) :: FnType tupType b -> tupType -> b

instance AppTuple () b where
  type FnType () b = b
  f $# () = f

instance AppTuple t2 b => AppTuple (t1, t2) b where
  type FnType (t1, t2) b = t1 -> (FnType t2 b)
  f $# (t1, t2) = f t1 $# t2

type App1       a =            (a, ())
type App2     a b =        (a, (b, ()))
type App3   a b c =    (a, (b, (c, ())))
type App4 a b c d = (a,(b, (c, (d, ()))))

app1 ::                a -> App1       a
app2 ::           a -> b -> App2     a b
app3 ::      a -> b -> c -> App3   a b c
app4 :: a -> b -> c -> d -> App4 a b c d

app1       a =             (a, ())
app2     a b =         (a, (b, ()))
app3   a b c =     (a, (b, (c, ())))
app4 a b c d = (a, (b, (c, (d, ()))))

_z :: String
_z = fn1 $# ("fred", (7, (True, ())))
  where
  fn1 :: String -> Int -> Bool -> String
  fn1 s n b = s ++ show n ++ show b
