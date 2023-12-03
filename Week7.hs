-- Binary Trees

Data BT a = Empty | Fork a (BT a) (BT a)

mirror :: BT a -> BT a
mirror Empty = Empty
mirror (Fork x l r) = x (mirror r) (mirror l)
