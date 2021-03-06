module Data.RedBlack.Internal where

-- From:
-- https://www.cs.kent.ac.uk/people/staff/smk/redblack/rb.html
--
data Color = R | B deriving (Eq, Show)

data Tree a = E | T Color (Tree a) a (Tree a) deriving (Eq, Show)

-- Invariants
-- 1. No red node has a red parent (equilvalently no red node has red children)
-- 2. Every path from the root node to an empty node contains the same number of black nodes
-- 3. The root and leaves of the tree are black

-- | Simple Tree operations
empty :: Tree a
empty = E

{- Insertion and membership test as by Okasaki -}
insert :: Ord a => a -> Tree a -> Tree a
insert x tree =
  T B lt v rt
  where
  T _ lt v rt = ins tree
  ins E = T R E x E
  ins s@(T col a y b)
    | x<y = balance col (ins a) y b
    | x>y = balance col a y (ins b)
    | otherwise = s
  -- ins s@(T R a y b)
  --   | x<y = T R (ins a) y b
  --   | x>y = T R a y (ins b)
  --   | otherwise = s

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T _ a y b)
  | x<y = member x a
  | x>y = member x b
  | otherwise = True

{- balance: first equation is new,
   to make it work with a weaker invariant -}
balance :: Color -> Tree a -> a -> Tree a -> Tree a
-- balance B (T R a x b) y (T R c z d) = T R (T B a x b) y (T B c z d)
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance col a x b = T col a x b

{- deletion a la SMK -}
delete :: Ord a => a -> Tree a -> Tree a
delete x t =
  case del t of {T _ a y b -> T B a y b; _ -> E}
  where
  del E = E
  del (T _ a y b)
      | x<y = delformLeft a y b
      | x>y = delformRight a y b
            | otherwise = app a b
  delformLeft a@(T B _ _ _) y b = balleft (del a) y b
  delformLeft a y b = T R (del a) y b
  delformRight a y b@(T B _ _ _) = balright a y (del b)
  delformRight a y b = T R a y (del b)

balleft :: Tree a -> a -> Tree a -> Tree a
balleft (T R a x b) y c = T R (T B a x b) y c
balleft bl x (T B a y b) = balance B bl x (T R a y b)
balleft bl x (T R (T B a y b) z c) = T R (T B bl x a) y (balance B b z (sub1 c))
balleft _ _ _ = error "unexpected input"

balright :: Tree a -> a -> Tree a -> Tree a
balright a x (T R b y c) = T R a x (T B b y c)
balright (T B a x b) y bl = balance B (T R a x b) y bl
balright (T R a x (T B b y c)) z bl = T R (balance B (sub1 a) x b) y (T B c z bl)
balright _ _ _ = error "unexpected input"

sub1 :: Tree a -> Tree a
sub1 (T B a x b) = T R a x b
sub1 _ = error "invariance violation"

app :: Tree a -> Tree a -> Tree a
app E x = x
app x E = x
app (T R a x b) (T R c y d) =
  case app b c of
      T R b' z c' -> T R (T R a x b') z (T R c' y d)
      bc -> T R a x (T R bc y d)
app (T B a x b) (T B c y d) =
  case app b c of
      T R b' z c' -> T R (T B a x b') z (T B c' y d)
      bc -> balleft a x (T B bc y d)
app a (T R b x c) = T R (app a b) x c
app (T R a x b) c = T R a x (app b c)

--
-- Testing functions
--


height :: Tree a -> Int
height E            = 0
height (T _ t _ t') = 1 + max (height t) (height t')

valid :: Tree a -> Bool
valid t = redNodesHaveBlackChildren t && allEqual (blackNodes t)
  where
    redNodesHaveBlackChildren = go
      where
        go (T R t1 _ t2) = colorOf t1 /= R && colorOf t2 /= R
        go _ = True

    colorOf E = B
    colorOf (T col _ _ _ ) = col

    allEqual [] = True
    allEqual (x:xs) = all (==x) xs

--
-- returns a list containing values, one for each leaf node. Each value
-- is the number of black nodes seen on the way to that leaf.
--
blackNodes :: Tree a -> [Int]
blackNodes = go 0
  where
    numBlack B = 1
    numBlack R = 0
    go n E = [n + 1]
    go n (T col t1 _ t2) = go (numBlack col + n) t1 ++ go (numBlack col + n) t2