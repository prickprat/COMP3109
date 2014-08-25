import Prelude
    hiding  ( Maybe (..)
            , foldr, filter, insert, sum, length
            , first, last, lookup, elem, replicate)

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs)
            = if p x 
                then x : filter p xs
                else filter p xs

replace :: (a -> Bool) -> a -> [a] -> [a]
replace p e (x : xs) = if p x   then e : xs
                                else x : replace p e xs

data Tree a
    = Leaf
    | Node a (Tree a) (Tree a)
    deriving Show

flatten :: Tree a -> [a]
flatten Leaf = []
flatten (Node v l r) = flatten l ++ v : flatten r

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node v l r) = if (x >= v) then Node v l (insert x r)
                                    else Node v (insert x l) r

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []       = z
foldr f z (x : xs) = f x (foldr f z xs)

-- And some examples
sum :: Num a => [a] -> a
sum    = foldr (+) 0
--
length :: [a] -> Int
length = foldr (\x y -> 1 + y) 0

elem :: Eq a => a -> [a] -> Bool
elem e = foldr (\x y -> x == e || y) False  


data Maybe a
         = Nothing
         | Just a
         deriving Show


class Monoid a where
    mzero   :: a
    mappend :: a -> a -> a

instance Monoid (Maybe a) where
    mzero   = Nothing

    mappend (Just x) (Just y) = Just x
    mappend (Just x) Nothing  = Just x
    mappend Nothing  (Just y) = Just y
    mappend _        _        = Nothing

lookup :: Eq k => k -> [(k, v)] -> Maybe v
lookup k = foldr (\x y -> if k == fst x then Just (snd x)
                                        else y) Nothing

unfold :: (s -> Maybe (a, s)) -> s -> [a]
unfold f s = case f s of
                    Nothing         -> []
                    Just (x, s')    -> x : unfold f s'

 
replicate :: Int -> a -> [a]
replicate n y = unfold (\x -> if x < n then Just (y, x + 1)
                                        else Nothing) 0

range :: Int -> Int -> [Int]
range start fin = unfold (\x -> if x <= fin then Just (x, x + 1)
                                                else Nothing ) start

prod :: Num a => [a] -> a
prod = foldr (*) 1

hfac :: Int -> Int
hfac n  = prod (range 1 n)
---------------------------------------------------------------------------
--
tree1 = Node 5 (Node 1 Leaf Leaf) 
               (Node 8 (Node 7 Leaf Leaf) 
                                      (Node 9 Leaf Leaf))
