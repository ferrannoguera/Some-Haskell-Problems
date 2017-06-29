myLength::[Int]->Int
myLength [] = 0
myLength (x:xs) = 1+(myLength xs)


myMaximum::[Int]->Int
myMaximum (x:xs) = (getMaximum xs x)

getMaximum::[Int]->Int->Int
getMaximum [] a = a
getMaximum (x:xs) a
  | x > a = (getMaximum xs x)
  | otherwise = (getMaximum xs a)



average :: [Int] -> Float
average x = fromIntegral (div (sum x) (myLength x))::Float


buildPalindrome :: [Int] -> [Int]
buildPalindrome x  = (reverse x)++x


elimina::[Int]->Int->[Int]
elimina [] y = []
elimina (x:xs) y = if (x == y) then (elimina xs y)
                               else ([x]++(elimina xs y))


remove :: [Int] -> [Int] -> [Int]
remove x [] = x
remove x (y:ys) = (remove (elimina x y) ys)


flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x++(flatten xs)


oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens x = (impars x,pars x)

impars::[Int] -> [Int]
impars [] = []
impars (x:xs)
  |mod x 2 == 1 = [x]++(impars xs)
  |otherwise = impars xs

pars::[Int] -> [Int]
pars [] = []
pars (x:xs)
  |mod x 2 == 0 = [x]++(pars xs)
  |otherwise = pars xs
  
  
isPrime :: Int -> Bool 
isPrime 0 = False
isPrime 1 = False
isPrime n = and [ mod n x /= 0 | x <- [2..(n-1)]]

  
primeDivisors :: Int -> [Int]
primeDivisors 0 = []
primeDivisors 1 = []
primeDivisors n = [x | x <- [2..n], mod n x == 0, isPrime x]
