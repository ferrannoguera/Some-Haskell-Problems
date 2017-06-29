ones :: [Integer]
ones = 1:ones


nats :: [Integer]
nats = iterate (+1) 0


ints::[Integer]
ints = [0]++[y | x<-tail nats, y<-[x,-x]]


triangulars::[Integer]
triangulars = [x | x<-nats, x<-[(x*(x+1)) `div` 2]]


factorials::[Integer]
factorials = scanl (*) 1 (filter (/=0) nats)


fibs::[Integer]
fibs = 0:scanl (+) 1 fibs


isPrime :: Integer -> Bool 
isPrime 0 = False
isPrime 1 = False
isPrime n = and [ mod n x /= 0 | x <- [2..(n-1)]]


primes :: [Integer]
primes = [x | x<-nats, isPrime x]


hammings :: [Integer]
hammings = 1:map (*2) hammings `merge` map (*3) hammings `merge` map (*5) hammings


merge (x:xs) (y:ys)
    |x < y = x : merge xs (y:ys)
    |x > y = y : merge (x:xs) ys
    |otherwise = x : merge xs ys
