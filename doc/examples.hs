{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Data.Foldable (fold)
import MapWith

data MyTraversable a = MyTraversable a a a a
  deriving (Eq, Show, Functor, Foldable, Traversable)

listThings :: Traversable t => t String -> String
listThings = fold . withFirstLast listThing
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

putItems :: (Traversable t, Show a) => t a -> IO ()
putItems = sequence_ . mapWith (putItem ^-> eltIx <-^ isLim)
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
