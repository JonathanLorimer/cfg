module Tree.Append where

import Data.Functor
import Data.Tree (Tree (..))

-- TODO: Come up with better names

appendLeaf :: (a -> b) -> ([a] -> b) -> [a] -> Tree a -> Tree b
appendLeaf f g acc (Node label []) = Node (f label) [Node (g (label : acc)) []]
appendLeaf f g acc (Node label xs) = Node (f label) $ fmap (appendLeaf f g (label : acc)) xs

mayAppendLeaf :: (a -> b) -> ([a] -> Maybe b) -> [a] -> Tree a -> Tree b
mayAppendLeaf f g acc (Node label []) =
  case g (label : acc) of
    Just leaf -> Node (f label) [Node leaf []]
    Nothing -> Node (f label) []
mayAppendLeaf f g acc (Node label xs) = Node (f label) $ fmap (mayAppendLeaf f g (label : acc)) xs

appendLeafA :: (Applicative f) => ([a] -> f a) -> [a] -> Tree a -> Tree (f a)
appendLeafA f acc = appendLeaf pure f acc

mayAppendLeafA :: (Applicative f) => ([a] -> Maybe (f a)) -> [a] -> Tree a -> Tree (f a)
mayAppendLeafA f acc = mayAppendLeaf pure f acc

mayAppendLeafA' :: (Applicative f) => ([a] -> f (Maybe a)) -> [a] -> Tree a -> f (Tree a)
mayAppendLeafA' f acc (Node label []) =
  f (label : acc)
    <&> \case
      Just leaf -> Node label [Node leaf []]
      Nothing -> Node label []
mayAppendLeafA' f acc (Node label xs) =
  let
    trees = traverse (mayAppendLeafA' f (label : acc)) xs
  in
    Node label <$> trees

travAppendLeafA :: (Applicative f) => ([a] -> f a) -> [a] -> Tree a -> f (Tree a)
travAppendLeafA f acc = sequenceA . appendLeaf pure f acc

travMayAppendLeafA :: (Traversable f, Applicative f) => ([a] -> f (Maybe a)) -> [a] -> Tree a -> f (Tree a)
travMayAppendLeafA f acc = sequenceA . mayAppendLeaf pure (sequenceA . f) acc
