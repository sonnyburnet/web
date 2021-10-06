module Data.Tree.Extended
       ( module Data.Tree
       , Data.Tree.Extended.insert
       ) where

import Data.Tree 
import Data.Tree.Zipper
import Control.Applicative

insert :: Eq a => a -> (a -> Bool) -> Tree a -> Tree a
insert node _ tree | isRoot (fromTree tree) && label (fromTree tree) == node = tree
insert node f tree = maybe tree (toTree . modifyTree (modifyT node) . fst) (search 1 f (fromTree tree)) 
  where modifyT node x = x { subForest = Node node [] : subForest x }

search :: Eq a => Int -> (a -> Bool) -> TreePos Full a -> Maybe (TreePos Full a, Int)
search i f node = go
  where
    go | f (label node) = Just (node, i)
       | otherwise = foldr seq empty (getChildren node)
    seq el els = search (i + 1) f el <|> els

getChildren :: TreePos Full a -> [TreePos Full a]
getChildren = go 0 []
  where
    go i xs node =
      case childAt i node of
        Just x -> go (i + 1) (x:xs) node
        Nothing -> reverse xs