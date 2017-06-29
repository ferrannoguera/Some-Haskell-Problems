flatten :: [[Int]] -> [Int]
flatten = foldl (++) []


myLength :: String -> Int
myLength = foldl (\acc x -> acc+1) 0

myReverse :: [Int] -> [Int]
myReverse = foldl (\acc x -> x:acc) []


countIn :: [[Int]] -> Int -> [Int] 
countIn x a = map(\x -> length (filter ((==) a) x)) x
--representa que les funcions \ son funcions dins de la funcio ("d'un sol us")
--representa que aqui es fa la funcio de comptar en la primera llista i llavors en el map es reparteix an els [[]]


firstWord :: String -> String 
firstWord x = takeWhile (\x-> x/=' ') (filtrar x)

filtrar::String->String
filtrar str = drop (length $ (takeWhile(\x-> x==' ') str)) str
