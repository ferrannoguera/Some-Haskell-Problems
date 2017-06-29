mergelist :: Ord a => [[a]] -> [a]
mergelist = foldl merge []


merge::Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
    |x<y = x : (merge xs (y:ys))
    |x>y = y : (merge (x:xs) ys)
    |otherwise = x : (merge xs ys)
    
    
mults :: [Integer] -> [Integer]
mults [] = []
mults l = 1: mergelist (map(\x ->map (*x) (mults l)) l )

