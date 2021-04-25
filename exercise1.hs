
-- #### EXERCICE 1 ####

-- 1.a.i
    getFirstParam:: Bool -> a -> a -> a
    getFirstParam True x y  = x
    getFirstParam False x y = y
    {- 
        Erkläerung:
        Diese Funktion gibt abhängig vom ersten Parameter (True/False) entweder den zweiten Parameter oder den dritten Parameter zurück.
    -}

-- 1.a.ii
    getFirstElem:: [Int] -> [Bool] -> Int
    getFirstElem [] _ = 0
    getFirstElem _ [] = 0
    getFirstElem (x:xs) (True:ys) = x
    getFirstElem (x:xs) (False:ys) = -1
    {- 
        Erkläerung:
        Diese Funktion bekommt eine Integer-Liste und eine Bool-Liste und gibt das erste Element der Int-Liste zurück, wenn
        das erste Element in der Bool-Liste True ist. Sonst -1 bzw. leere Liste, falls eine der Listen leer ist.
    -}

-- 1.a.iii
    listMapper::[a] -> (a -> b) -> [b]
    listMapper [] f = []
    listMapper (x:xs) f = [f x] ++ listMapper xs f
    {- 
        Erkläerung:
        Diese Funktion bekommt eine Liste und eine funtion als Paraemeter und bildet diese Liste mithilfe dieser Funktion ab und gibt
        die resultierende Liste zurück.
    -}


-- 1.b

{- 
    Gegeben:
    f::Bool -> [a] -> a 
    length::[b] -> Int


    \x y -> f ((length [(f True [x])]) > 0) [y]

    \x y -> f ((length [(f True [x])]) > 0) [y]




    2) \x y -> f ((length [(Type a)]) > 0) [y]
    3) \x y -> f ((Type Int) > 0) [y]
    4) \x y -> f (Bool) [y]
    5) \x y -> f (Bool) (Type y)
    6) \x y -> f (Bool) [y] => -- Type ist unbestimmt, ist immer vom "Type y"
 

#### EXERCICE 2 ####

-- 2.a.i
    [x] = (x:y) ++ z
    [1] = (1:[]) ++ []
    => 
        x = 1;
        y = z = []

-- 2.a.ii
    (x:z):[y] = [x ++ y]
    ([]:[]):[] = []

    ++ kombiniert 2 Listen daher wird [x ++ y] zu [[...][...]]. Dann müssten x, y und (x:z) auch eine Liste sein.
    Dann hat die Linke Seite des Ausdrucks eine Listentiefe von 3, die rechte aber 2. Failed!


-- 2.a.iii
    [x] ++ [[y]] ++ z =  = x:[y]:z

    2 + Listentiefe(y) != 1 + Listenteife von y => FAILED



-- 2.b

    p1) (x:xs):xss
    p2) x:y:xs

    0:[]:[] = [[0]]
    (0:[]):[] = [[0]]

    


-}

-- #### EXERCICE 3 ####

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


-- 3.b / Nach Vorlesung

    is_in_list :: Int -> [Int] -> Bool
    is_in_list _ []  = False
    is_in_list s (x:xs)     | s == x  = True
                            | otherwise = is_in_list s xs

    intersectVL :: [Int] -> [Int] -> [Int]
    intersectVL [] _  = []
    intersectVL  _ [] = []
    intersectVL (x:xs) ys       | is_in_list x ys = x:intersectVL xs ys
                                | otherwise = intersectVL xs ys

-- 3.c
    sortList :: [Int] -> [Int]
    sortList [] = []
    sortList (x:xs) =   sortList ([y | y <- xs, y <= x]) ++     -- List with elements less than x
                        [x] ++                                  -- element x
                        sortList ([y | y <- xs, y > x])         -- List with elements greater than x

    sortedIntersect:: [Int] -> [Int] -> [Int]
    sortedIntersect l1 l2 = sortList(intersect l1 l2)

    -- / Nach Vorlesung
    insertSorted:: Int -> [Int] -> [Int]
    insertSorted x [] = x:[]
    insertSorted x (y:ys)   | x<=y = x:y:ys
                            | otherwise = y:insertSorted x ys

    sortListVL :: [Int] -> [Int]
    sortListVL [] = []
    sortListVL (x:xs) = insertSorted x (sortListVL xs)
    

-- 3.d
    dropOddInds :: [a] -> [a]
    dropOddInds []  = []
    dropOddInds [x] = [x]
    dropOddInds (x:y:list) = [x] ++ dropOddInds list
