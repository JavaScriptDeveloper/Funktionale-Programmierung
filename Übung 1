
-- 3.a
    digitSum:: Int->Int
    digitSum 0 = 0
    digitSum x = x `mod` 10 + digitSum(x `div`10)

-- 3.b
    intersect :: [Int] -> [Int] -> [Int]
    intersect [] _  = []
    intersect  _ [] = []
    intersect  l1 (y:l2) =  [x | x <- l1, x == y] ++ 
                            intersect l1 l2

-- 3.c
    sortList :: [Int] -> [Int]
    sortList [] = []
    sortList (x:xs) =   sortList ([y | y <- xs, y <= x]) ++     -- List with Elements less than x
                        [x] ++                                  -- element x
                        sortList ([y | y <- xs, y > x])         -- List with Elements greater than x

    sortedIntersect:: [Int] -> [Int] -> [Int]
    sortedIntersect l1 l2 = sortList(intersect l1 l2)

-- 3.d

    dropOddInds :: [a] -> [a]
    dropOddInds []  = []
    dropOddInds [x] = [x]
    dropOddInds (x:y:list) = [x] ++ dropOddInds list
