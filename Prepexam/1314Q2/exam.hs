inflists :: [[Integer]]
inflists = [(iterate (+1) x) | x<-[1..]]


takeLESum::Integer -> [Integer] -> [Integer]
takeLESum _ [] = []
takeLESum s (x:xs)
  | x <= s    = x : takeLESum (s-x) xs
  | x > s = takeLESum (s) xs
  | otherwise = []
  
  

consecutSum::Integer -> [Integer]
consecutSum x = takeLESum x ([z | y<-inflists, z<-y])


dc :: (a -> Bool) -> (a -> b) -> (a -> [a]) -> (a -> [b] -> b) -> a -> b
dc isTrivial solveTrivial divide conquer problem
    | isTrivial problem = solveTrivial problem
    | otherwise = conquer problem sols
    where sols = map (dc isTrivial solveTrivial divide conquer) (divide problem)
          
          
isJust :: Maybe [a] -> [a]
isJust (Just x) = x
isJust (Nothing) = []


enrique :: String -> (Either String Int) 
enrique "enrique" = Left "hola"
enrique "hola enrique" = Right 45234


isLeft :: (Either String Int) -> String
isLeft (Left s) = s
