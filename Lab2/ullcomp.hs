myMap :: (a -> b) -> [a] -> [b]
myMap f x = [f xs | xs<-x]


myFilter :: (a -> Bool) -> [a] -> [a] 
myFilter f x = [xs | xs<-x, f xs]


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f x y = [f xs ys | (xs,ys)<-zip x y]



thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify x y = [(xs,ys) | xs<-x,ys<-y, mod xs ys == 0]



factors :: Int -> [Int] 
factors x = [xs | xs<-[1..x], mod x xs == 0]
