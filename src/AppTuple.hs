{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module AppTuple
  (
    AppTuple(..)
  )
where

import Data.Tuple.OneTuple

class AppTuple tupType b where
  type FnType tupType b :: *
  ($#) :: FnType tupType b -> tupType -> b

instance AppTuple () b where
  type FnType () b = b
  f $# () = f

instance AppTuple t2 b => AppTuple (t1, t2) b where
  type FnType (t1, t2) b = t1 -> (FnType t2 b)
  f $# (t1, t2) = f t1 $# t2

instance AppTuple (OneTuple a) b where
  type FnType (OneTuple a) b = a -> b
  f $# (OneTuple a) = f a

_z :: String
_z = fn1 $# ("fred", (7, (True, ())))
  where
  fn1 :: String -> Int -> Bool -> String
  fn1 s n b = s ++ show n ++ show b
