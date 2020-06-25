{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.Function ((&))
import MapWith

--An example Traversable used in the examples below.
data MyTraversable a = MyTraversable a a a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

--Use of pre-packaged withFirstLast.

listThings :: Traversable t => t String -> String
listThings = foldMapWith $ listThing & isFirst & isLast
  where
  listThing thing True _    =            thing
  listThing thing _    True = " and " ++ thing
  listThing thing _    _    = ", "    ++ thing
  
{-
listThings ["foo", "bar", "baz"]
"foo, bar and baz"

listThings (Just "foo")
"foo"

listThings (MyTraversable "foo" "bar" "baz" "bob")
"foo, bar, baz and bob"
-}

--Use of a custom map taking the element index (from the start) and an isLast (isLim from right) indicator.

putItems :: (Traversable t, Show a) => t a -> IO ()
putItems = mapWithM_ (putItem ^-> eltIx <-^ isLim)
  where
  putItem x n isLast = putStrLn $ show (n+1) ++ ": " ++ show x ++ if isLast then " (last)" else ""

{-
putItems ["foo", "bar", "baz"]
1: "foo"
2: "bar"
3: "baz" (last)

putItems (Just "foo")
1: "foo" (last)

putItems (MyTraversable "foo" "bar" "baz" "bob")
1: "foo"
2: "bar"
3: "baz"
4: "bob" (last)
-}


--Example that returns the same type of structure.

justFirstLast :: Traversable t => t a -> t (Maybe a)
justFirstLast = withFirstLast whenFirstLast
  where
  whenFirstLast x True _    = Just x
  whenFirstLast x _    True = Just x
  whenFirstLast x _    _    = Nothing

{-
justFirstLast ["foo", "bar", "baz"]
[Just "foo",Nothing,Just "baz"]

justFirstLast (Just "foo")
Just (Just "foo")

justFirstLast (MyTraversable "foo" "bar" "baz" "bob")
MyTraversable (Just "foo") Nothing Nothing (Just "bob")
-}

