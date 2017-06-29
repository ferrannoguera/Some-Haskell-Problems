absValue::Int->Int
absValue n = if n < 0 then n*(-1)
                      else n
                      
                      
power::Int->Int->Int
power _ 0 = 1
power n m = n*(power n (m-1))



isqrt::Int->Int
isqrt x = floor $ sqrt $ fromIntegral x


isPrime :: Int -> Bool 
isPrime 0 = False
isPrime 1 = False
isPrime n = and [ mod n x /= 0 | x <- [2..(n-1)]]



slowFib::Int->Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = ((slowFib (n-2))+(slowFib (n-1)))



quickFib::Int->Int
quickFib n = fib !! n  
    where fib = 0 : 1: zipWith (+) fib (tail fib) 
