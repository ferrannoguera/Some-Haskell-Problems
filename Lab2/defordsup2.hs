countIf :: (Int -> Bool) -> [Int] -> Int
countIf f x = length $ filter f x


pam :: [Int] -> [Int -> Int] -> [[Int]]
pam x f = [[f x| x<-x] | f<-f]


pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 x f = [[fs xs | fs<-f] | xs<-x]


filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl filt fold n x = foldl fold n (filter filt x)


insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f [] y = [y]
insert f (x:xs) y
    |f x y = [x]++insert f xs y
    |otherwise = [y]++[x]++xs
    
    
insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f x = insertionSort2 f x []

insertionSort2 :: (Int -> Int -> Bool) -> [Int] -> [Int] -> [Int]
insertionSort2 _ [] ys = ys
insertionSort2 f (x:xs) ys = insertionSort2 f xs (insert f ys x)
