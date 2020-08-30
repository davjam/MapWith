{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Main (main)
where

import System.Exit

import CurryTF

main | and tests = exitSuccess
     | otherwise = exitFailure
     
tests = [ zC == 3081
        , zU == 3081
        , fn1 ('x' :# 3 :# True :# HNil) == (uncurryN . curryN) fn1 ('x' :# 3 :# True :# HNil)
        , fn2 'y' 2 False == (curryN . uncurryN @(HList '[Char, Int, Bool]) @String) fn2 'y' 2 False
        ]

fn1 :: HList '[Char, Int, Bool] -> String
fn1 (c :#  n :# b :# HNil) = c : replicate n '1' ++ if b then "hello" else "goodbye"

fn2 :: Char -> Int -> Bool -> String
fn2 c n b = c : replicate n '2' ++ if b then "hello" else "goodbye"

xC    a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z
      a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1
      a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2
      = a +b +c +d +e +f +g +h +i +j +k +l +m +n +o +p +q +r +s +t +u +v +w +x +y +z
      + a1+b1+c1+d1+e1+f1+g1+h1+i1+j1+k1+l1+m1+n1+o1+p1+q1+r1+s1+t1+u1+v1+w1+x1+y1+z1
      + a2+b2+c2+d2+e2+f2+g2+h2+i2+j2+k2+l2+m2+n2+o2+p2+q2+r2+s2+t2+u2+v2+w2+x2+y2+z2

zC :: Int
zC = xC  $# (1 :# 2 :# 3 :# 4 :# 5 :# 6 :# 7 :# 8 :# 9 :# 10:# 11:# 12:# 13:# 14:# 15:# 16:# 17:# 18:# 19:# 20:# 21:# 22:# 23:# 24:# 25:# 26 :#
             27:# 28:# 29:# 30:# 31:# 32:# 33:# 34:# 35:# 36:# 37:# 38:# 39:# 40:# 41:# 42:# 43:# 44:# 45:# 46:# 47:# 48:# 49:# 50:# 51:# 52 :#
             53:# 54:# 55:# 56:# 57:# 58:# 59:# 60:# 61:# 62:# 63:# 64:# 65:# 66:# 67:# 68:# 69:# 70:# 71:# 72:# 73:# 74:# 75:# 76:# 77:# 78 :#
             HNil)

xU :: (HList '[Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,
               Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,
               Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int])
      -> Int
xU  (a :#b :#c :#d :#e :#f :#g :#h :#i :#j :#k :#l :#m :#n :#o :#p :#q :#r :#s :#t :#u :#v :#w :#x :#y :#z :#
     a1:#b1:#c1:#d1:#e1:#f1:#g1:#h1:#i1:#j1:#k1:#l1:#m1:#n1:#o1:#p1:#q1:#r1:#s1:#t1:#u1:#v1:#w1:#x1:#y1:#z1:#
     a2:#b2:#c2:#d2:#e2:#f2:#g2:#h2:#i2:#j2:#k2:#l2:#m2:#n2:#o2:#p2:#q2:#r2:#s2:#t2:#u2:#v2:#w2:#x2:#y2:#z2:#
     HNil)
    = a +b +c +d +e +f +g +h +i +j +k +l +m +n +o +p +q +r +s +t +u +v +w +x +y +z
    + a1+b1+c1+d1+e1+f1+g1+h1+i1+j1+k1+l1+m1+n1+o1+p1+q1+r1+s1+t1+u1+v1+w1+x1+y1+z1
    + a2+b2+c2+d2+e2+f2+g2+h2+i2+j2+k2+l2+m2+n2+o2+p2+q2+r2+s2+t2+u2+v2+w2+x2+y2+z2

zU :: Int
zU = curryN xU   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
                27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52
                53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78

