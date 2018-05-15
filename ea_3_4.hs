data Color = R | B | BB
data RBTree a = Empty | BBEmpty | Node Color (RBTree a) a (RBTree a)

delete :: Ord a => RBTree a -> a -> RBTree a
delete t x = blackenRoot (del t x) where
  del Empty _ = Empty
  del (Node color l k r) x
    | x < k = fixDB color (del l x) k r
    | x > k = fixDB color l k (del r x)
    | isEmpty l = if color == B then makeBlack r else r
    | isEmpty r = if color == B then makeBlack l else l
    | otherwise = fixDB color l k' (del r k') where k' = min r

  blackenRoot (Node _ l k r) = Node B l k r
  blackenRoot _ = Empty

  makeBlack (Node B l k r) = Node BB l k r
  makeBlack (Node _ l k r) = Node B l k r
  makeBlack Empty = BBEmpty
  makeBlack t = t

