module RedBlackTree
    (
      Tree,
      delete,
      is_member,
      insert,
      emptyTree,
      filterT
    ) where


data Color = Red | Black deriving (Show)



data Tree a = Leaf | Node a Color (Tree a) (Tree a) deriving (Show)


color :: Tree a -> Color
color Leaf = Black
color (Node _ c _ _) = c


emptyTree:: Tree a
emptyTree = Leaf

-- Функция для проверки данных в структуре
is_member :: (Ord a) => a -> Tree a -> Bool
is_member x Leaf = False
is_member x (Node d _ l r)
    | x < d     = is_member x l
    | x == d    = True
    | otherwise = is_member x r

-- Вставка
insert :: (Ord a) => a -> Tree a -> Tree a
insert x s = makeBlack $ ins s
  where ins Leaf  = Node x Red Leaf Leaf
        ins (Node d c l r)
          | x < d  = balance d c (ins l) r
          | x == d = Node d c l r
          | x > d  = balance d c l (ins r)
        makeBlack (Node d _ l r) = Node d Black l r

-- балансировка при вставке (fix B->R->R)
balance :: a -> Color -> Tree a -> Tree a -> Tree a
balance z Black (Node y Red (Node x Red a b) c) d = Node y Red (Node x Black a b) (Node z Black c d)
balance z Black (Node x Red a (Node y Red b c)) d = Node y Red (Node x Black a b) (Node z Black c d)
balance x Black a (Node z Red (Node y Red b c) d) = Node y Red (Node x Black a b) (Node z Black c d)
balance x Black a (Node y Red b (Node z Red c d)) = Node y Red (Node x Black a b) (Node z Black c d)
balance x c a b = Node x c a b

balance' :: Tree a -> Tree a
balance' (Node color left value right) = balance color left value right
-- Удаление
delete :: (Ord a) => a -> Tree a -> Tree a
delete x t = makeBlack $ del x t
  where makeBlack (Node y _ a b) = Node y Black a b
        makeBlack Leaf           = Leaf

del :: (Ord a) => a -> Tree a -> Tree a
del _ Leaf = Leaf
del x t@(Node y _ l r)
  | x < y = delL x t
  | x > y = delR x t
  | otherwise = merge l r


delL :: Ord a => a -> Tree a -> Tree a
delL x (Node y _ t1@(Node _ Black _ _) t2) = balL $ Node y Black (del x t1) t2
delL x (Node  y _ t1 t2) = Node y Red (del x t1) t2

balL :: Tree a -> Tree a
balL (Node y Black (Node x Red t1 t2) t3) = Node y Red (Node x Black t1 t2) t3
                                                            -- fix this case:    B
                                                            --                  /\
                                                            --                 R t1
                                                            --                /\
                                                            --               t2 t3

balL (Node y Black t1 (Node z Black t2 t3)) = balance' (Node y Black t1 (Node z Red t2 t3))
                                                            -- fix this case:    B
                                                            --                  /\
                                                            --                 t1 B
                                                            --                   /\
                                                            --                 t2 t3

balL (Node y Black t1 (Node z Red (Node u Black t2 t3) t4@(Node value Black l r))) =
  Node u Red (Node y Black t1 t2) (balance' (Node z Black t3 (Node value Red l r)))
                                                            -- fix this case:    B
                                                            --                  /\
                                                            --                 t1  R
                                                            --                    /\
                                                            --                  t2 t3

delR :: Ord a => a -> Tree a -> Tree a
delR x (Node y _ t1 t2@(Node _ Black _ _)) = balR $ Node y Black t1 (del x t2)
delR x (Node y _ t1 t2) = Node y Red t1 (del x t2)

balR :: Tree a -> Tree a
balR (Node y Black t1 (Node x Red t2 t3)) = Node y Red t1(Node x Black t2 t3)
balR (Node y Black (Node z Black t1 t2) t3) = balance' (Node y Black (Node z Red t1 t2) t3)
balR (Node y Black (Node z Red (Node value Black l r) (Node u Black t2 t3)) t4) =
  Node u Red (balance' (Node z Black (Node value Red l r) t2)) (Node y Black t3 t4)

merge :: Tree a -> Tree a -> Tree a
merge Leaf t = t
merge t Leaf = t
merge t1@(Node _ Black _ _) (Node y Red t3 t4) = Node y Red (merge t1 t3) t4
merge (Node x Red t1 t2) t3@(Node _ Black _ _) = Node x Red t1 (merge t2 t3)
merge (Node x Red t1 t2) (Node y Red t3 t4)  =
  let s = merge t2 t3
  in case s of
        (Node z Red s1 s2) -> (Node z Red (Node x Red t1 s1) (Node y Red s2 t4))
        (Node _ Black _ _) -> (Node x Red t1 (Node y Red s t4))
merge (Node x Black t1 t2) (Node y Black t3 t4)  =
  let s = merge t2 t3
  in case s of
        (Node z Red s1 s2) -> (Node z Red (Node x Black t1 s1) (Node y Black s2 t4))
        (Node z Black s1 s2) -> balL (Node x Black t1 (Node y Black s t4))


instance (Ord a) => Eq (Tree a) where
  (Leaf) == (Leaf) = True
  (Node d1 _ _ _) == (Node d2 _ _ _) = d1 == d2

instance (Ord a) => Ord (Tree a) where
  (Node d1 _ _ _) `compare` (Node d2 _ _ _) = d1 `compare` d2

instance (Ord a) => Semigroup (Tree a) where
  (<>) = mappend

instance (Ord a) => Monoid (Tree a) where
  mempty = Leaf
  l1 `mappend` l2 = foldl (\x y ->insert y x) l1 l2


instance Foldable Tree where
  foldr _ z Leaf = z
  foldr f z (Node d _ l r) = foldr f (f d (foldr f z r)) l
  foldl _ z Leaf = z
  foldl f z (Node d _ l r) = foldl f (f (foldl f z l) d) r

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node d c l r) = Node (f d) c (fmap f l ) (fmap f r)

filterT :: (a -> Bool) -> Tree a -> [a]
filterT _ Leaf = []
filterT f (Node d _ l r) | f d = [d] ++ (filterT f l ) ++ (filterT f r)
                         | otherwise = (filterT f l ) ++ (filterT f r)
