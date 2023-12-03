-- Binary Trees

data BT a = Empty
          | Fork a (BT a) (BT a) deriving (Show, Read, Eq, Ord)


mirror :: BT a -> BT a
mirror Empty = Empty
mirror (Fork x l r) = Fork x (mirror r) (mirror l)

size :: BT a -> Integer
size Empty = 0
size (Fork x l r) = 1 + (size l) + (size r)

leaves :: BT a -> Integer
leaves Empty = 0
leaves (Fork x l r) = leaves l + leaves r

height :: BT a -> Integer
height Empty = 0
height (Fork x l r) = 1 + max (height l) (height r)
