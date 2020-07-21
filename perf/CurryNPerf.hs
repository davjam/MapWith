import CurryN

main = do
  print $ fn 37 29 83 14
        + fn $# app4 38 26 81 28 
        + fn $# (39, (23, (47, (22, ()))))

fn :: Int -> Int -> Int -> Int -> Int
fn w x y z | w > 10 = fn (w - 6) (x - 15) (y - 7) (z - 8)
           | otherwise = w + x + y + z

{-
after:
ghc G.hs -fforce-recomp -O2 -ddump-simpl -dsuppress-all > G.core

CurryNPerf.core has (after "INLINEing" app1..4):
main2
  = case $wfn 37# 29# 83# 14# of ww_s3wI { __DEFAULT ->
    case $wfn 38# 26# 81# 28# of ww1_X3xC { __DEFAULT ->
    case $wfn 39# 23# 47# 22# of ww2_X3xG { __DEFAULT ->
    case $wshowSignedInt 0# (+# (+# ww_s3wK ww1_X3xE) ww2_X3xI) [] of ...

So I'pretty conviced they compile to the same thing.

CurryNPerf.prof adds confusion:
                                                                              individual      inherited
COST CENTRE   MODULE                   SRC                 no.     entries  %time %alloc   %time %alloc
 ...
 CAF          Main                     <entire-module>                   50          0    0.0    0.0     0.0    0.4      0         0
  main        Main                     perf\CurryNPerf.hs:(3,1)-(6,42)   84          1    0.0    0.4     0.0    0.4      0       184
   fn         Main                     perf\CurryNPerf.hs:(9,1)-(10,38)  86          6    0.0    0.0     0.0    0.0      0         0
   $#         CurryN                   src\CurryN.hs:112:1-29            87          2    0.0    0.0     0.0    0.0      0         0
    uncurryN  CurryN                   src\CurryN.hs:108:3-56            88          8    0.0    0.0     0.0    0.0      0         0
     fn       Main                     perf\CurryNPerf.hs:(9,1)-(10,38)  90         12    0.0    0.0     0.0    0.0      0         0
     uncurryN CurryN                   src\CurryN.hs:102:3-19            89          2    0.0    0.0     0.0    0.0      0         0

Though I'm sure I remember reading somewhere that this is normal (i.e. that profiling reports on functions that have been optimised away, or something).
But I can't find the web page right now.

Without the INLINE pragmas, the app4 doesn't evaporate, but the $# does:
  = case $wfn 37# 29# 83# 14# of ww_s418 { __DEFAULT ->
    case main_args1 of { (arg1_au0, moreArgs1_au1) ->
    case moreArgs1_au1 of { (arg2_XuG, moreArgs2_XuI) ->
    case moreArgs2_XuI of { (arg3_XuQ, moreArgs3_XuS) ->
    case moreArgs3_XuS of { (arg4_Xv0, moreArgs4_Xv2) ->
    case arg1_au0 of { I# ww2_X42c ->
    case arg2_XuG of { I# ww4_X42l ->
    case arg3_XuQ of { I# ww6_X42u ->
    case arg4_Xv0 of { I# ww8_X42D ->
    case $wfn ww2_X42c ww4_X42l ww6_X42u ww8_X42D of ww9_X42K
    { __DEFAULT ->
    case let {
           f_s3Xs
           f_s3Xs = I# ww9_X42K } in
         case moreArgs4_Xv2 of { () -> f_s3Xs }
    of
    { I# y_a32R ->
    case $wfn 39# 23# 47# 22# of ww10_X42y { __DEFAULT ->
    case $wshowSignedInt 0# (+# (+# ww_s418 y_a32R) ww10_X42y) [] of ...
-}
