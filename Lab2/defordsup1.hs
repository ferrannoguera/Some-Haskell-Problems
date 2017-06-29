myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ a [] = a
myFoldl f a (x:xs) = myFoldl f (f a x) xs



myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (x:xs) = f x (myFoldr f b xs)


myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : (myIterate f $ f a)


myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil f g a
  | f a 	= a			--si es compleix f, STOP WAIT A MINUT
  | otherwise	=  myUntil f g ( g a)   --si no es compleix f, lets keep going people
  
  
myMap :: (a -> b) -> [a] -> [b]
myMap f x = [f xs | xs<-x]


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f x = [xs | xs<-x, f xs]
                           
                           
myAll :: (a -> Bool) -> [a] -> Bool
myAll f x = myFoldr g True (x)
	where g b a = (f b) && a
	      	      
myAny :: (a -> Bool) -> [a] -> Bool
myAny f x = ajuda g False (x)
	where g a b = a || (f b)
	      
	      
ajuda :: (Bool -> b -> Bool) -> Bool -> [b] -> Bool
ajuda f a [] = a
ajuda f a (x:xs) 
	| f a x = True
	| otherwise	= ajuda f (f a x) xs 
    
    
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = [(x,y)] ++ myZip xs ys


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f x y =  [f xs ys | (xs,ys) <- zip x y]
