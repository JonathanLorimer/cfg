module Tree.Append where

import Data.Tree (Tree (..))

appendLeaf :: (a -> b) -> ([a] -> b) -> [a] -> Tree a -> Tree b
appendLeaf f g acc (Node label []) = Node (f label) [Node (g (label:acc)) []]
appendLeaf f g acc (Node label xs) = Node (f label) $ fmap (appendLeaf f g (label:acc)) xs

appendLeafA :: (Applicative f) => ([a] -> f a) -> [a] -> Tree a -> Tree (f a)
appendLeafA f acc = appendLeaf pure f acc

travAppendLeafA :: (Applicative f) => ([a] -> f a) -> [a] -> Tree a -> f (Tree a)
travAppendLeafA f acc = sequenceA . appendLeaf pure f acc

