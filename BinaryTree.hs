module BinaryTree where

import           Data.Maybe (isNothing)

data BinaryTree a =
  None |
  Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

testTree :: BinaryTree Integer
testTree = Node (Node None 3 None) 1 (Node None 4 None)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert val None = Node None val None
insert val (Node left curr right)
  | val == curr = Node left val right
  | val < curr  = Node (insert val left) curr right
  | otherwise  = Node left curr (insert val right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ None = None
mapTree f (Node left curr right) = Node (mapTree f left) (f curr) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder None                   = []
preorder (Node left curr right) = [curr] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder None                   = []
inorder (Node left curr right) = preorder left ++ [curr] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder None                   = []
postorder (Node left curr right) = preorder left ++ preorder right ++ [curr]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b None = b
foldTree f b tree = foldr f b (inorder tree)

foldTree' :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree' _ b None = b
foldTree' f b (Node left curr right) = f curr (foldTree' f b left) (foldTree' f b right)

unfold :: (b -> Maybe (b, a, b)) -> b -> BinaryTree a
unfold f b
  | isNothing (f b) = None
  | otherwise = Node (unfold f l) a (unfold f r)
  where
    Just (l, a, r) = f b

treeBuild :: Integer -> BinaryTree Integer
treeBuild i = unfold f (min i 0)
  where
    f x
      | x < 0 = Nothing
      | x < i = Just (x+1, x, x+1)
      | otherwise = Just (-1, x, -1)
