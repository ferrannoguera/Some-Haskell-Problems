-- Problema 1
esDob :: Integer -> Integer -> Bool
esDob = (\x y -> (div x 2) == y)


-- Problema 2
elimDiv :: [Integer] -> [Integer]
elimDiv [] = []
elimDiv x = (aux [] x)

aux :: [Integer] -> [Integer] -> [Integer]
aux xs [] = xs
aux [] (y:ys) = (aux [y] ys)
aux xs (y:ys)
    |(null laux) = (aux (xs++[y]) ys)
    |otherwise = (aux xs ys)
    where laux = [z | z<-xs, (mod z y == 0) || (mod y z == 0)]
          
          
          
-- Problema 3
infGen :: (Int -> Int) -> [Int]
infGen f = 1:(map f (infGen f))


novafunc :: Int -> (Int -> Int) -> Int
novafunc x f = last $ take x $ (infGen f)
