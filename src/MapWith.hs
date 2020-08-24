{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : MapWith
-- Description : Provides the fmap-like functions
-- Copyright   : (c) David James, 2020
-- License     : BSD3
-- Stability   : Experimental
--
-- Provides 'fmap'-like functionality, but can also "inject" additional parameters to the mapping function, such as:
--
-- * whether the first / last item
-- * the previous / next item
-- * the index from the start / end

module MapWith
  (
  -- * Type Names
  -- $TypeNames

  -- * Pre-Packaged Maps
  -- $PrePackagedMaps
    withFirstLast
  , andFirstLast
  , withPrevNext
  , andPrevNext

  -- * Custom Maps
  -- $CustomMaps
  , mapWith
  , mapWithM
  , mapWithM_
  , foldMapWith
  , InjectedFn
  , Injectable(..)

  -- * Predefined Injectors
  -- $PredefinedInjectors
  , isLim
  , adjElt
  , adj2Elts
  , eltIx
  , evenElt
  , foldlElts
  , foldl1Elts
  , eltFrom
  , eltFromMay
  , eltFromDef

  -- ** Pre-Combined Injectors
  -- $PrecombinedInjectors
  , isFirst
  , isLast
  , prevElt
  , nextElt

  -- * Custom Injectors
  , Injector(..)
  
  -- ** Stacked-Tuple Helpers
  -- $StackedTupleHelpers
  , module CurryTFExps  --these are very helpful for building Injectors. Someone can import CurryTF if they want more.
  )
where

import CurryTF
import qualified CurryTF as CurryTFExps (App1, App2, App3, App4, app1, app2, app3, app4)

import Data.Foldable (fold)
import Data.Traversable (mapAccumL, mapAccumR)
import Data.Function ((&))
import Control.Exception (assert)
import GHC.Base (build)

-- $TypeNames
-- These names are used for types and variables throughout:
--
-- [@t@]: the 'Traversable' we're mapping over
-- [@a@]: a value in the input 'Traversable'
-- [@b@]: a result in the output 'Traversable'
-- [@i@]: an output from an 'Injector', injected into a map function. (i may represent one than one injected value).
-- [@s@]: the internal state in an 'Injector'

--XXXX I'd like to add separate comments for each parameter, but that's not supported to GHC 8.6 https://github.com/haskell/haddock/issues/836#issuecomment-391402361
data Injector a i = forall s. Injector (a -> s -> (s, i)) s -- ^the first parameter is a generate function, the second parameter is the initial state.

-- ^ Injectors have an initial state and a generate function.
--
--  For each item in the 'Traversable', the generate function can use both:
--
--  - the item from the 'Traversable', and
--  - the current state
--
--  to determine both:
--
--  - the new state, and
--  - the injection value(s)
--
--  The injection value(s) must be an @args@ (per 'CurryTF'), in order for the injector to work with the  '^->' and '<-^' operators.
--  These can be created by:
--
--  - using 'app1', 'app2', etc; or
--  - by nesting the values appropriately e.g @(i1, ())@ or @(i1, (i2, (i3, (i4, (i5, .. () ..)))))@
--  - defining a new instance of 'CurryTF'
--
--  The first value(s) to inject is/are determined by a first call to the generate function.
--  The first call to the generate function is with the first (if combined with '^->') or last (if combined with '<-^') item from the 'Traversable' and the initial state.
--
--  For example:
--
--  >>> funnyNext a s = (a + 1, app1 $ a + s)
--  >>> funnyInjector = Injector funnyNext 17
--  >>> mapWith ((\_ i -> i) ^-> funnyInjector) [4,8,3]
--  [21,13,12]
--
--  +-------+---------------+------+-----------------+---------------+
--  + Call  + Initial State + Item + New State       + Injection     +
--  +=======+===============+======+=================+===============+
--  + 1     + 17            + 4    + 4+1=5           + 17+4=__21__   +
--  +-------+---------------+------+-----------------+---------------+
--  + 2     + 5             + 8    + 8+1=9           + 5+8=__13__    +
--  +-------+---------------+------+-----------------+---------------+
--  + 3     + 9             + 3    + 3+1=4 (ignored) | 9+3=__12__    +
--  +-------+---------------+------+-----------------+---------------+
--
--  >>> mapWith ((\_ i -> i) <-^ funnyInjector) [4,8,3]
--  [13,12,20]
--
--  +-------+---------------+------+-----------------+---------------+
--  + Call  + Initial State + Item + New State       + Injection     +
--  +=======+===============+======+=================+===============+
--  + 1     + 17            + 3    + 3+1=4           + 17+3=__20__   +
--  +-------+---------------+------+-----------------+---------------+
--  + 2     + 4             + 8    + 8+1=9           + 4+8=__12__    +
--  +-------+---------------+------+-----------------+---------------+
--  + 3     + 9             + 4    + 4+1=5 (ignored) | 9+4=__13__    +
--  +-------+---------------+------+-----------------+---------------+
--
--  More usefully, this might allow for e.g. injection of random values, etc.

-- $StackedTupleHelpers
-- These make it easier to define 'Injector' types and injection values.

{-# INLINE injPair #-}
injPair :: Injector a i1 -> Injector a i2 -> Injector a (i1, i2)
injPair (Injector n1 z1) (Injector n2 z2) = Injector nxt (z1, z2)
  where
  nxt a ~(s1, s2) = let (s1', i1) = n1 a s1       -- !! NOTE THE ~ !! It allows "constant" injectors (e.g. isLim), and hence e.g. andFirstLast to work on infinite lists.
                        (s2', i2) = n2 a s2
                    in ((s1', s2'), (i1, i2))

-- $PredefinedInjectors
-- 
-- #PredefinedInjectors#Use these (or custom 'Injector's) to create 'InjectedFn's that can be used with 'mapWith'

{-# INLINABLE isLim #-}
isLim :: Injector a (App1 Bool)
isLim = Injector (\_ i -> (False, app1 i)) True
-- ^ inject 'True' if the item is at the limit:
--
-- - from the left: if it's the first item
-- - from the right: if it's the last item
--
-- else inject False.
--
-- >>> let f = (\a b -> [a, ch b]); ch isLim = if isLim then '*' else ' ' in mapWith (f ^-> isLim) "12345"
-- ["1*","2 ","3 ","4 ","5 "]
-- >>> let f = (\a b -> [a, ch b]); ch isLim = if isLim then '*' else ' ' in mapWith (f <-^ isLim) "12345"
-- ["1 ","2 ","3 ","4 ","5*"]

{-# INLINABLE eltIx #-}
eltIx :: Integral i => Injector a (App1 i)
eltIx = Injector (\_ i -> (i+1, app1 i)) 0
-- ^ inject the item index:
--
-- - from the left: the first item is 0, the second 1, etc.
-- - from the right: the last item is 0, the penultimate 1, etc.
--
-- >>> let f = (\a b -> [a, head $ show b]) in mapWith (f ^-> eltIx) "freddy"
-- ["f0","r1","e2","d3","d4","y5"]
-- >>> let f = (\a b -> [a, head $ show b]) in mapWith (f <-^ eltIx) "freddy"
-- ["f5","r4","e3","d2","d1","y0"]

evenElt :: Injector a (App1 Bool)
evenElt = Injector (\_ s -> (not s, app1 s)) True
-- ^ True if an even-numbered (0th, 2nd, 4th, etc) item, counting from the left or from the right.
--
-- >>> let f c e = [c, if e then '*' else ' '] in mapWith (f ^-> evenElt) "123456"
-- ["1*","2 ","3*","4 ","5*","6 "]
-- >>> let f c e = [c, if e then '*' else ' '] in mapWith (f <-^ evenElt) "123456"
-- ["1 ","2*","3 ","4*","5 ","6*"]

{-# INLINABLE eltFrom #-}
eltFrom :: [i]          -- ^ The elements to inject. There must be enough elements.
        -> Injector a (App1 i)
eltFrom l = Injector (\_ s -> assert (not $ null s) (tail s, app1 $ head s)) l
-- ^ Inject each given element in turn:
--
-- - from the left: the first element will be injected for the first item in the 'Traversable'.
-- - from the right: the first element will be injected for the last item in the 'Traversable'.
--
-- >>> let f a b = [a,b] in mapWith (f ^-> eltFrom "bill") "sue"
-- ["sb","ui","el"]
-- >>> let f a b = [a,b] in mapWith (f <-^ eltFrom "bill") "sue"
-- ["sl","ui","eb"]
--
-- As a result of laziness, it is not always an error if there are not enough elements, for example:
--
-- >>> drop 1 $ mapWith ((\_ i -> i) <-^ eltFrom [8,2]) "abc"
-- [2,8]

{-# INLINABLE eltFromMay #-}
eltFromMay :: [i] -> Injector a (App1 (Maybe i))
eltFromMay l = Injector (\_ s -> case s of []   -> ([], app1 Nothing)
                                           i:ix -> (ix, app1 $ Just i))
                         l
-- ^ a safe version of `eltFrom`. Injects 'Just' each given element in turn, or 'Nothing' after they've been exhausted.
--
-- >>> let f a b = [a,ch b]; ch = maybe '-' id in mapWith (f ^-> eltFromMay "ben") "sally"
-- ["sb","ae","ln","l-","y-"]
-- >>> let f a b = [a,ch b]; ch = maybe '-' id in mapWith (f <-^ eltFromMay "ben") "sally"
-- ["s-","a-","ln","le","yb"]

{-# INLINABLE eltFromDef #-}
eltFromDef :: i -> [i] -> Injector a (App1 i)
eltFromDef def l = Injector (\_ s -> case s of []   -> ([], app1 def)
                                               i:ix -> (ix, app1 i))
                            l
-- ^ a safe version of `eltFrom`. Injects each given element in turn, or the default after they've been exhausted.
--
-- >>> let f a b = [a,b] in mapWith (f ^-> eltFromDef 'X' "ben") "sally"
-- ["sb","ae","ln","lX","yX"]
-- >>> let f a b = [a,b] in mapWith (f <-^ eltFromDef 'X' "ben") "sally"
-- ["sX","aX","ln","le","yb"]

{-# INLINABLE adjElt #-}
adjElt :: Injector a (App1 (Maybe a))
adjElt = Injector (\a prevMay -> (Just a, app1 prevMay)) Nothing
-- ^ inject 'Just' the adjacent item:
--
-- - from the left: the previous item, except for the first item
-- - from the right: the next item, except for the last item. (The "previous from the right" is the "next".)
--
-- inject 'Nothing' if there is no adjacent item (i.e. for the first / last).
--
-- >>> let f a b = [a,ch b]; ch = maybe '-' id in mapWith (f ^-> adjElt) "12345"
-- ["1-","21","32","43","54"]
-- >>> let f a b = [a,ch b]; ch = maybe '-' id in mapWith (f <-^ adjElt) "12345"
-- ["12","23","34","45","5-"]

{-# INLINABLE adj2Elts #-}
adj2Elts :: Injector a (App2 (Maybe a) (Maybe a))
adj2Elts = Injector (\a (prev1May, prev2May) -> ((Just a, prev1May), app2 prev1May prev2May)) (Nothing, Nothing)
-- ^ like 'adjElt', but injects the two adjacent items into separate parameters.
--
-- >>> let f a b c = [a,ch b,ch c]; ch = maybe '-' id in mapWith (f ^-> adj2Elts) "12345"
-- ["1--","21-","321","432","543"]
-- >>> let f a b c = [a,ch b,ch c]; ch = maybe '-' id in mapWith (f <-^ adj2Elts) "12345"
-- ["123","234","345","45-","5--"]

{-# INLINABLE foldlElts #-}
foldlElts :: (i -> a -> i)
          -> i
          -> Injector a (App1 i)
foldlElts f z = Injector (\a s -> let s' = f s a in (s', app1 s')) z
-- ^ Inject a (left-associative) fold of the items:
--
-- +------+---------------------------------------------------------------------------------------------+
-- |      |                        Injected Value                                                       |
-- |      +---------------------------------------------+-----------------------------------------------+
-- | Item | from the left                               | from the right                                |
-- +======+=============================================+===============================================+
-- |  a0  | @z \`acc\` a0@                              | @((z \`acc\` an) \`acc\` .. a1) \`acc\` a0@   |
-- +------+---------------------------------------------+-----------------------------------------------+
-- |  a1  | @(z \`acc\` a0) \`acc\` a1@                 | @(z \`acc\` an) \`acc\` .. a1@                |
-- +------+---------------------------------------------+-----------------------------------------------+
-- |  ..  |                                             |                                               |
-- +------+---------------------------------------------+-----------------------------------------------+
-- |  an  | @((z \`acc\` a0) \`acc\` a1) \`acc\` .. an@ | @z \`acc\` an@                                |
-- +------+---------------------------------------------+-----------------------------------------------+
--
-- >>> let f a b = a ++ show b in mapWith (f ^-> foldlElts (\l s -> l + length s) 0) ["every", "good", "boy"]
-- ["every5","good9","boy12"]
-- >>> let f a b = a ++ show b in mapWith (f <-^ foldlElts (\l s -> l + length s) 0) ["every", "good", "boy"]
-- ["every12","good7","boy3"]

{-# INLINABLE foldl1Elts #-}
foldl1Elts :: (a -> a -> a)
           -> Injector a (App1 a)
foldl1Elts f = Injector (\a s -> let s' = maybe a (flip f a) s in (Just s', app1 s')) Nothing
-- ^ A variant of 'foldlElts' that has no starting value:
--
-- +------+----------------------------------------------------------------------+
-- |      |                        Injected Value                                |
-- |      +----------------------------------+-----------------------------------+
-- | Item | from the left                    | from the right                    |
-- +======+==================================+===================================+
-- |  a0  | @a0@                             | @(an \`acc\` .. a1) \`acc\` a0@   |
-- +------+----------------------------------+-----------------------------------+
-- |  a1  | @a0 \`acc\` a1@                  | @an \`acc\` .. a1@                |
-- +------+----------------------------------+-----------------------------------+
-- |  ..  |                                  |                                   |
-- +------+----------------------------------+-----------------------------------+
-- |  an  | @(a0 \`acc\` a1) \`acc\` .. an@  | @an@                              |
-- +------+----------------------------------+-----------------------------------+
--
-- >>> mapWith ((,) ^-> foldl1Elts (-)) [10,1,3]
-- [(10,10),(1,9),(3,6)]
-- >>> mapWith ((,) <-^ foldl1Elts (-)) [10,1,3]
-- [(10,-8),(1,2),(3,3)]

-- $CustomMaps
--
-- In general, a map function will take one parameter from the 'Traversable', then one (or more) from each of any number of 'Injector's. For example:
--
-- >>> mapFn w x y z = (w, x, y, z)
-- >>> injectedFn = mapFn <-^ isLim ^-> eltIx <-^ eltFrom [8,2,7,1]
-- >>> mapWith injectedFn "abc"
-- [('a',False,0,7),('b',False,1,2),('c',True,2,8)]
--
-- Where:
--
-- - @mapFn@: a function that maps over a 'Traversable', but requires additional parameters
-- - @injectedFn@: represents the combination of @mapFn@ with three injectors that provide the required parameters:
--
--     - @'<-^' 'isLim'@: injects True if this is the limit, from the right (i.e. the last item).
--     - @'^->' 'eltIx'@: inject the index, from the left
--     - @'<-^' 'eltFrom' [8,2,7,1]@: inject elements from this list, from the right.
--
-- 'mapWith' then maps the @mapFn@ over the 'Traversable', with the following parameters:
--
-- +------+--------+---------+---------+---------+
-- | Call | w      | x       | y       | z       |
-- +======+========+=========+=========+=========+
-- |    1 | \'a\'  | 'False' | 0       | 7       |
-- +------+--------+---------+---------+---------+
-- |    2 | \'b\'  | 'False' | 1       | 2       |
-- +------+--------+---------+---------+---------+
-- |    3 | \'c\'  | 'True'  | 2       | 8       |
-- +------+--------+---------+---------+---------+

{-# INLINABLE mapWith #-}
mapWith :: Traversable t
        => InjectedFn a b
        -> t a
        -> t b
mapWith (InjectedFnL  f (Injector gen z)) = mySnd . myMapAccumL acc z
  where acc s a = let (s', i) = gen a s in (s', f a i)
mapWith (InjectedFnR  f (Injector gen z)) = snd . mapAccumR acc z
  where acc s a = let (s', i) = gen a s in (s', f a i)
mapWith (InjectedFnLR f (Injector genL zL) (Injector genR zR)) =  snd . mapAccumR accR zR . mySnd . myMapAccumL accL zL
  --have mapAccumL create values (not parial function applications). It can fuse with data providers
  --the myMapAccumR can then PIN the mapAccumL results (not on stack) plus its own R-based injections.
  --(good) consumets can tail recurse/loop over the data applying the injections as they go.
  --(Don't store data or partial functions during recusion unwind: it can cause very slow behaviour on huge lists).
  where accL s a       = let (s', il) = genL a s in (s', (a, il))
        accR s (a, il) = let (s', ir) = genR a s in (s', f a il ir)
-- ^ maps an 'InjectedFn' over a 'Traversable' type @t@, turning a @t a@ into a @t b@ and preserving the structure of @t@.
--
-- Parameters (as defined in the 'InjectedFn') are passed to a map function (embedded in the 'InjectedFn'), in addition to the elements of the 'Traversable'.

mapWithM :: (Traversable t, Monad m) => InjectedFn a (m b) -> t a -> m (t b)
mapWithM f = sequence . mapWith f
-- ^ like 'mapM', but with an 'InjectedFn'.

mapWithM_ :: (Traversable t, Monad m) => InjectedFn a (m b) -> t a -> m ()
mapWithM_ f = sequence_ . mapWith f
-- ^ like 'mapM_' (which is like 'mapM' but ignores the results), but with an 'InjectedFn'.

foldMapWith :: (Traversable t, Monoid b) => InjectedFn a b -> t a -> b
foldMapWith f = fold . mapWith f
-- ^ like 'foldMap', but with an 'InjectedFn'

data InjectedFn a b
  = forall l r. InjectedFnLR (a -> l -> r -> b) (Injector a l) (Injector a r)
  | forall l  . InjectedFnL  (a -> l      -> b) (Injector a l)
  | forall   r. InjectedFnR  (a      -> r -> b)                (Injector a r)

-- ^ Represents a function from @a@, plus a number of injected values, to @b@.
--
-- Used by 'mapWith' (& related), which maps over a 'Traversable', injecting the additional values as it goes.
--
-- Constructed by combining  a map function with 'Injector's using the '^->' and '<-^' operators.
--
-- The sequence:
--
-- @(a -> i1 -> i2 -> ... -> in -> b) /op1/ /inj1/ /op2/ /inj2/ ... /opn/ /injn/@
--
-- where:
--
-- - each @/op/@ is '^->' or '<-^'; and
-- - each @/inj/@ is an 'Injector'
--
-- produces an @'InjectedFn' a b@, with n injected values (or more if any of the injectors inject multiple values).

class Injectable m where
  -- | Inject "from the left"
  (^->) :: CurryTF i b => m a (FnType i b) -> Injector a i -> InjectedFn a b
  -- | Inject "from the right"
  (<-^) :: CurryTF i b => m a (FnType i b) -> Injector a i -> InjectedFn a b

-- ^ An 'Injectable' is (recursively) either:
--
-- - a function @(a -> i1 [.. -> in] -> b)@; or
-- - an @InjectedFn a (i1 [.. -> in] -> b)@, created by @'Injectable' /op/ 'Injector'@
--
-- When @n@ is the number of parameters injected by an injector (most commonly 1).


infixl 1 ^->
infixl 1 <-^

--In the below, iL = existing left Injector; nL = new left Injector. l = existing left-injected value; nl = new left-injected value.

instance Injectable (->) where
  f                    ^-> nL = InjectedFnL  (\a     nl         -> f a     $# nl)             nL
  f                    <-^ nR = InjectedFnR  (\a            nr  -> f a     $# nr)                         nR

instance Injectable InjectedFn where
  InjectedFnL  f iL    ^-> nL = InjectedFnL  (\a (l, nl)        -> f a l   $# nl) (injPair iL nL)
  InjectedFnR  f    iR ^-> nL = InjectedFnLR (\a     nl  r      -> f a   r $# nl)             nL       iR
  InjectedFnLR f iL iR ^-> nL = InjectedFnLR (\a (l, nl) r      -> f a l r $# nl) (injPair iL nL)      iR

  InjectedFnL  f iL    <-^ nR = InjectedFnLR (\a  l         nr  -> f a l   $# nr)          iL             nR
  InjectedFnR  f    iR <-^ nR = InjectedFnR  (\a        (r, nr) -> f a   r $# nr)             (injPair iR nR)
  InjectedFnLR f iL iR <-^ nR = InjectedFnLR (\a  l     (r, nr) -> f a l r $# nr)          iL (injPair iR nR)

-- $PrecombinedInjectors
-- These are combinations of '^->' or '<-^' with [pre-defined injectors](#PredefinedInjectors).
--
-- They work well with the '&' operator, and can be combined with the '^->' and '<-^' operators e.g.:
--
-- prop> mapWith (f & isFirst <-^ eltFrom [9,2]) == mapWith (f ^-> isLim <-^ eltFrom [9,2])
--
-- You may find them more memorable or easier to type.

isFirst :: Injectable f => f a (Bool -> b) -> InjectedFn a b
isFirst f = f ^-> isLim
-- ^ 'isLim', from the left.

isLast :: Injectable f => f a (Bool -> b) -> InjectedFn a b
isLast f = f <-^ isLim
-- ^ 'isLim', from the right.

prevElt :: Injectable f => f a (Maybe a -> b) -> InjectedFn a b
prevElt f = f ^-> adjElt
-- ^ 'adjElt', from the left.

nextElt :: Injectable f => f a (Maybe a -> b) -> InjectedFn a b
nextElt f = f <-^ adjElt
-- ^ 'adjElt', from the right.

-- $PrePackagedMaps
-- Some pre-defined maps with commonly used injectors.

{-# INLINABLE withFirstLast #-}
withFirstLast :: Traversable t => (a -> Bool -> Bool -> b) -> t a -> t b
withFirstLast f = mapWith $ f & isFirst & isLast
-- ^ Maps over a 'Traversable', with additional parameters indicating whether an item is the first or last (or both) in the list.
--
-- >>> let f x isFirst isLast = star isFirst ++ x ++ star isLast; star b = if b then "*" else "" in withFirstLast f ["foo", "bar", "baz"]
-- ["*foo", "bar", "baz*"]

{-# INLINABLE andFirstLast #-}
andFirstLast :: Traversable t => t a -> t (a, Bool, Bool)
andFirstLast = withFirstLast (,,)
-- ^ > andFirstLast = withFirstLast (,,)

{-# INLINABLE withPrevNext #-}
withPrevNext :: Traversable t => (a -> Maybe a -> Maybe a -> b) -> t a -> t b
withPrevNext f = mapWith $ f & prevElt & nextElt
-- ^ Maps over a 'Traversable', with additional parameters indicating the previous and next elements.
--
-- The second (or third) parameter to the map function is 'Nothing' when called for the first (or last) item, otherwise it's 'Just' the previous (or next) item.
--
-- >>> let f x prvMay nxtMay = maybe "*" (cmp x) prvMay ++ x ++ maybe "*" (cmp x) nxtMay; cmp x y = show $ compare x y in withPrevNext f ["foo", "bar", "baz"]
-- ["*fooGT","LTbarLT","GTbaz*"]

{-# INLINABLE andPrevNext #-}
andPrevNext :: Traversable t => t a -> t (a, Maybe a, Maybe a)
andPrevNext = withPrevNext (,,)
-- ^ > andPrevNext = withPrevNext (,,)

-- myMapAccumL/R which can fuse.

{-# NOINLINE [1] myMapAccumL #-}  --cf {-# NOINLINE [1] unsafeTake #-}
myMapAccumL :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
myMapAccumL = mapAccumL

{-# NOINLINE [1] mySnd #-}
mySnd :: (a, b) -> b
mySnd = snd

{-# RULES --modelled on "take".
"sndMapAccumL" [~1]  forall f z xs. mySnd (myMapAccumL f z xs) =
  build (\c nil -> foldr (mapAccumLFB c f) (flipSeqMapAccumL nil) xs z)
 #-}
 {-
 I can't make this fire, but it doesn't seem to be a great loss.
"sndMapAccumLList" [1]  forall f z xs. foldr (mapAccumLFB (:) f) (flipSeqMapAccumL []) xs z = 
  sndMapAccumLList f z xs
{-# NOINLINE [1] sndMapAccumLList #-}
sndMapAccumLList :: (s -> a -> (s, b)) -> s -> [a] -> [b]
sndMapAccumLList f z = snd . mapAccumL f z
  -}

{-# INLINE [0] mapAccumLFB #-}
mapAccumLFB :: (b -> r -> r) -> (s -> a -> (s, b)) -> a -> (s -> r) -> s -> r
mapAccumLFB c f x xs = \s -> let (s', b) = f s x in b `c` xs s'

{-# INLINE [0] flipSeqMapAccumL #-}
flipSeqMapAccumL :: a -> s -> a
flipSeqMapAccumL x !_s = x

