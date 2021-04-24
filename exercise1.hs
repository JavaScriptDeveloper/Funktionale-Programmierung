
-- #### 1 ####

-- 1.a.i
    getFirstParam:: Bool -> a -> a -> a
    getFirstParam True x y  = x
    getFirstParam False x y = y
    {- 
        Erklärung:
        Diese Funktion gibt abhängig vom ersten Parameter (True/False) entweder den zweiten Parameter oder den dritten Parameter zurück.
    -}

-- 1.a.ii
    getFirstElem:: [Int] -> [Bool] -> Int
    getFirstElem [] _ = 0
    getFirstElem _ [] = 0
    getFirstElem (x:xs) (True:ys) = x
    getFirstElem (x:xs) (False:ys) = -1
    {- 
        Erklärung:
        Diese Funktion bekommt eine Integer-Liste und eine Bool-Liste und gibt das erste Element der Int-Liste zurück, wenn
        das erste Element in der Bool-Liste True ist. Sonst -1 bzw. leere Liste, falls eine der Listen leer ist.
    -}

-- 1.a.iii
    listMapper::[a] -> (a -> b) -> [b]
    listMapper [] f = []
    listMapper (x:xs) f = [f x] ++ listMapper xs f
    {- 
        Erklärung:
        Diese Funktion bekommt eine Liste und eine funtion als Paraemeter und bildet diese Liste mithilfe dieser Funktion ab und gibt
        die resultierende Liste zurück.
    -}


-- 1.b

{- 
    Gegeben:
    f::Bool -> [a] -> a 
    length::[b] -> Int
    

    \x y -> f ((length [(f True [x])]) > 0) [y]
    2) \x y -> f ((length [(Type a)]) > 0) [y]
    3) \x y -> f ((Type Int) > 0) [y]
    4) \x y -> f (Bool) [y]
    5) \x y -> f (Bool) (Type y)
    6) \x y -> f (Bool) [y] => -- Type ist unbestimmt, ist immer vom "Type y"
 -}


-- #### 3 ####

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
    sortList (x:xs) =   sortList ([y | y <- xs, y <= x]) ++     -- List with elements less than x
                        [x] ++                                  -- element x
                        sortList ([y | y <- xs, y > x])         -- List with elements greater than x
    sortedIntersect:: [Int] -> [Int] -> [Int]
    sortedIntersect l1 l2 = sortList(intersect l1 l2)

-- 3.d
    dropOddInds :: [a] -> [a]
    dropOddInds []  = []
    dropOddInds [x] = [x]
    dropOddInds (x:y:list) = [x] ++ dropOddInds list
