-- genPairs :: Eq a => [a] -> [a] -> [a] -> [(a,a)]







data Arbre a = Node a (Arbre a) (Arbre a) | Abuit deriving (Show)


ttake :: Int -> Arbre a -> Arbre a
ttake 0 _= Abuit
ttake _ Abuit = Abuit
ttake x (Node y esq dre) = (Node y (ttake (x-1) esq) (ttake (x-1) dre))


inftree :: (Num a) => Arbre a
inftree = inf' 1
  where inf' n = let next = inf' (n+1) in Node n next next
        
        
data ErrList a = Err Int [a]


instance Eq a => Eq (ErrList a) where
    (Err x xs) == (Err y ys) = (getDiffs xs ys (min x y))
    
    
--no m'acaba d'agradar, PERO ES AIXI JODERDEDEU

getDiffs :: Eq a => [a] -> [a] -> Int -> Bool
getDiffs [] y z = length y <= z
getDiffs x [] z = length x <= z
getDiffs (x:xs) (y:ys) z
    |z < 0 = False
    |x == y = (getDiffs xs ys z)
    |otherwise = (getDiffs xs ys (z-1))
