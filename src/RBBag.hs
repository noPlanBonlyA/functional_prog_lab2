{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module RBBag
  ( RBBag (..),
    Color (..),
    insert',
    remove',
    map',
    foldl'',
    foldr'',
    lookup',
    fromList',
    filter',
    insertMany,
  )
where

data Color = Red | Black deriving (Show, Eq)

data RBBag a
  = Leaf
  | Node
      { element :: a,
        count :: Int,
        color :: Color,
        leftChild :: RBBag a,
        rightChild :: RBBag a
      }
  deriving (Show)

instance (Ord a) => Eq (RBBag a) where
  (==) a b =
    and $
      zipWith
        (\(el1, cnt1) (el2, cnt2) -> el1 == el2 && cnt1 == cnt2)
        (foldr'' join [] a)
        (foldr'' join [] b)
    where
      join (el, cnt) acc = (el, cnt) : acc

instance (Ord a) => Semigroup (RBBag a) where
  (<>) a b = foldr'' (\(el, cnt) acc -> insertMany el cnt acc) b a

instance (Ord a) => Monoid (RBBag a) where
  mempty = Leaf

fromList' :: (Ord a) => [(a, Int)] -> RBBag a
fromList' ls = go ls Leaf
  where
    go [] d = d
    go ((x, cnt) : xs) d = go xs (insert' x d)

lookup' :: (Ord a) => a -> RBBag a -> Maybe Int
lookup' _ Leaf = Nothing
lookup' el Node {element = nel, count = cnt, leftChild = left, rightChild = right}
  | el == nel = Just cnt
  | el < nel = lookup' el left
  | otherwise = lookup' el right

insert' :: (Ord a) => a -> RBBag a -> RBBag a
insert' el d = makeBlack $ insertHelper el d

insertHelper :: (Ord a) => a -> RBBag a -> RBBag a
insertHelper el Leaf = Node el 1 Red Leaf Leaf
insertHelper el node@Node {element = e, count = c, leftChild = left, rightChild = right}
  | el < e = balance (node {leftChild = insertHelper el left})
  | el > e = balance (node {rightChild = insertHelper el right})
  | otherwise = node {count = c + 1}

balance :: RBBag a -> RBBag a
balance (Node z zv Black (Node y yv Red (Node x xv Red a b) c) d) = Node y yv Red (Node x xv Black a b) (Node z zv Black c d)
balance (Node z zv Black (Node x xv Red a (Node y yv Red b c)) d) = Node y yv Red (Node x xv Black a b) (Node z zv Black c d)
balance (Node x xv Black a (Node z zv Red (Node y yv Red b c) d)) = Node y yv Red (Node x xv Black a b) (Node z zv Black c d)
balance (Node x xv Black a (Node y yv Red b (Node z zv Red c d))) = Node y yv Red (Node x xv Black a b) (Node z zv Black c d)
balance n = n

balance' :: RBBag a -> RBBag a
balance' Leaf = Leaf
balance' (Node x xv clr left right) = balance (Node x xv clr left right)

makeBlack :: RBBag a -> RBBag a
makeBlack (Node k v _ left right) = Node k v Black left right
makeBlack Leaf = Leaf

remove' :: (Ord a) => a -> RBBag a -> RBBag a
remove' k dict = makeBlack $ del k dict

del :: (Ord a) => a -> RBBag a -> RBBag a
del _ Leaf = Leaf
del k node@Node {element = nk, leftChild = left, rightChild = right}
  | k < nk = delL k node
  | k > nk = delR k node
  | otherwise = fuse left right

balL :: RBBag a -> RBBag a
balL (Node y yv Black (Node x xv Red t1 t2) t3) = Node y yv Red (Node x xv Black t1 t2) t3
balL (Node y yv Black t1 (Node z zv Black t2 t3)) = balance' (Node y yv Black t1 (Node z zv Red t2 t3))
balL (Node y yv Black t1 (Node z zv Red (Node u uv Black t2 t3) (Node t tval Black l r))) = Node u uv Red (Node y yv Black t1 t2) (balance' (Node z zv Black t3 (Node t tval Red l r)))
balL node = node

balR :: RBBag a -> RBBag a
balR (Node y yv Black t1 (Node x xv Red t2 t3)) = Node y yv Red t1 (Node x xv Black t2 t3)
balR (Node y yv Black (Node z zv Black t1 t2) t3) = balance' (Node y yv Black (Node z zv Red t1 t2) t3)
balR (Node y yv Black (Node z zv Red (Node t tv Black l r) (Node u uv Black t2 t3)) t4) = Node u uv Red (balance' (Node z zv Black (Node t tv Red l r) t2)) (Node y yv Black t3 t4)
balR node = node

delL :: (Ord a) => a -> RBBag a -> RBBag a
delL k (Node y yv _ t1@(Node _ _ Black _ _) t2) = balL $ Node y yv Black (del k t1) t2
delL k (Node y yv _ t1 t2) = Node y yv Red (del k t1) t2
delL _ Leaf = Leaf

delR :: (Ord a) => a -> RBBag a -> RBBag a
delR k (Node y yv _ t1 t2@(Node _ _ Black _ _)) = balR $ Node y yv Black t1 (del k t2)
delR k (Node y yv _ t1 t2) = Node y yv Red t1 (del k t2)
delR _ Leaf = Leaf

fuse :: RBBag a -> RBBag a -> RBBag a
fuse Leaf t = t
fuse t Leaf = t
fuse t1@(Node _ _ Black _ _) (Node y yv Red t3 t4) = Node y yv Red (fuse t1 t3) t4
fuse (Node x xv Red t1 t2) t3@(Node _ _ Black _ _) = Node x xv Red t1 (fuse t2 t3)
fuse (Node x xv Red t1 t2) (Node y yv Red t3 t4) =
  let s = fuse t2 t3
   in case s of
        (Node z zv Red s1 s2) -> Node z zv Red (Node x xv Red t1 s1) (Node y yv Red s2 t4)
        (Node _ _ Black _ _) -> Node x xv Red t1 (Node y yv Red s t4)
        Leaf -> Node x xv Red t1 (Node y yv Red Leaf t4)
fuse (Node x xv Black t1 t2) (Node y yv Black t3 t4) =
  let s = fuse t2 t3
   in case s of
        (Node z zv Red s1 s2) -> Node z zv Red (Node x xv Black t1 s1) (Node y yv Black s2 t4)
        (Node _ _ Black _ _) -> balL (Node x xv Black t1 (Node y yv Black s t4))
        Leaf -> balL (Node x xv Black t1 (Node y yv Red Leaf t4))

map' :: (Ord b) => (a -> b) -> RBBag a -> RBBag b
map' _ Leaf = Leaf
map' f node@Node {element = el, count = cnt, leftChild = left, rightChild = right} =
  Node (f el) cnt (color node) (map' f left) (map' f right)

foldl'' :: (Ord a) => ((a, Int) -> b -> b) -> b -> RBBag a -> b
foldl'' _ acc Leaf = acc
foldl'' f acc (Node {element = el, count = cnt, leftChild = left, rightChild = right}) = foldl'' f (f (el, cnt) (foldl'' f acc left)) right

foldr'' :: (Ord a) => ((a, Int) -> b -> b) -> b -> RBBag a -> b
foldr'' _ acc Leaf = acc
foldr'' f acc (Node {element = el, count = cnt, leftChild = left, rightChild = right}) = foldr'' f (f (el, cnt) (foldr'' f acc right)) left

insertMany :: (Ord a) => a -> Int -> RBBag a -> RBBag a
insertMany el cnt d = makeBlack $ insertManyHelper el cnt d

insertManyHelper :: (Ord a) => a -> Int -> RBBag a -> RBBag a
insertManyHelper el cnt Leaf = Node el cnt Red Leaf Leaf
insertManyHelper el cnt node@Node {element = e, count = c, leftChild = left, rightChild = right}
  | el < e = balance (node {leftChild = insertManyHelper el cnt left})
  | el > e = balance (node {rightChild = insertManyHelper el cnt right})
  | otherwise = node {count = cnt}

filter' :: (Ord a) => ((a, Int) -> Bool) -> RBBag a -> RBBag a
filter' p = foldr'' (\(k, c) d -> if p (k, c) then insertMany k c d else d) mempty
