--Exercise Sheet 2

--Exercise 1

--a)

data IndexedTree a = Node [(IndexedTree a)] a| Leaf [a] a deriving Show

--b)

treeToList :: IndexedTree a -> [a]
treeToList (Leaf xs _) = xs
treeToList (Node (x:xs) y) = treeToList x ++ nodelistToList xs
 where 
  nodelistToList :: [IndexedTree a] -> [a]
  nodelistToList [] = []
  nodelistToList (x:xs) = treeToList x ++ nodelistToList xs
  
--Test
--treeToList Node [Node [Leaf [3,7,8] 3, Leaf [12,23] 12, Leaf [26] 26] 3, Node [ Leaf [32,43,98] 32, Leaf [101] 101] 32] 3

--c)

--contains :: Ord a => a -> IndexedTree a -> Bool
--TODO


--Exercise 2

--a)

--b)

--c)

--d)

--Exercise 3

--a)

conjunction :: [Bool] -> Bool
conjunction [] = True
conjunction (x:xs) = x && conjunction xs

--b)

universalQ :: (a -> Bool) -> [a] -> Bool
universalQ f [] = True
universalQ f xs = conjunction (map f xs)

--Exercise 4
data MultTree a = MultNode a [MultTree a] deriving Show

zipWithMult :: (a -> b -> c) -> MultTree a -> MultTree b -> MultTree c
zipWithMult f (MultNode a xs) (MultNode b ys) = MultNode (f a b) 
 (zipWith (zipWithMult f) xs ys)

--Test
--t1 = MultNode 8 [MultNode 3 [ MultNode (-56) [], MultNode 4 [], MultNode 987 []], MultNode 4 [MultNode 6 []]]
--t2 = MultNode (-2) [MultNode 5 [MultNode 16 [],MultNode 7 []], MultNode (-9) [MultNode 1 [], MultNode 5 []]]
-- zipWithMult (+) t1 t2


--Exercise 5

--a)

--b)


--Exercise 6




--Exercise 7


--a)

--b)

--c)

--d)

--e)*
