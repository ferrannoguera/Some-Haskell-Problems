insert::[Int]->Int->[Int]
insert x y = [x | x<-x, x<=y] ++ [y] ++ [x| x<- x, x > y]

isort :: [Int] -> [Int]
isort x = isort2 x []

isort2 :: [Int] -> [Int] -> [Int]
isort2 [] y = y
isort2 (x:xs) y = (isort2 xs (insert y x))


remove:: [Int] -> Int-> [Int]
remove (x:xs) y
  |x /= y = [x] ++ remove xs y
  |otherwise = xs

ssort::[Int]->[Int]
ssort [] = []
ssort x = [y] ++ (ssort (remove x y))
	  where y = minimum x


merge:: [Int] -> [Int] -> [Int]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
  |x < y = [x] ++ (merge xs ([y]++ys))
  |x >= y = [y] ++ (merge ([x]++xs) ys)

msort:: [Int]->[Int]
msort [] = []
msort [x] = [x]
msort x = (merge (msort (take (length x `div` 2) x)) (msort (drop (length x `div` 2) x)))


qsort ::[Int]->[Int] 
qsort [] = []  
qsort (x:xs) =   
    let smallerSorted = qsort [a | a <- xs, a <= x]  
        biggerSorted = qsort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  


gentQinsert::Ord a=>[a]->a->[a]
gentQinsert x y = [x | x<-x, x<=y] ++ [y] ++ [x| x<- x, x > y]

genQsort::Ord a =>[a]->[a]
genQsort x = gentQsort2 x []

gentQsort2::Ord a =>[a]->[a]->[a]
gentQsort2 [] y = y
gentQsort2 (x:xs) y = (gentQsort2 xs (gentQinsert y x))
