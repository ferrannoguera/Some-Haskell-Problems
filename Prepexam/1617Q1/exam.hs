-- PROBLEMA 1

mergelist :: Ord a => [[a]] -> [a]
mergelist = foldl merge []
 
merge::Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
  |x < y = [x] ++ (merge xs ([y]++ys))
  |x > y = [y] ++ (merge ([x]++xs) ys)
  |otherwise = [x] ++ (merge xs ys) 

-- PROBLEMA 2

mults :: [Integer] -> [Integer]
mults [] = []
mults l = 1: mergelist (map(\x ->map (*x) (mults l)) l )


-- PROBLEMA 3

-- Exercici 3.1

data Procs a = Binary (a->a->a) (Procs a) | Unary (a -> a) (Procs a) | Skip (Procs a) | End

--Exercici 3.2

exec :: [a] -> (Procs a) -> [a]
exec x End = x
exec (x:xs) (Skip y) = [x]++(exec xs y)
exec (x:xs) (Unary f y) = (exec ([(f x)]++xs) y)
exec (x:xs) (Binary f y) 
  |(null xs) = (exec ([f x x]++xs) y) 
  |otherwise = (exec ([f x (head xs)]++(tail xs)) y)

  

-- PROBLEMA 4

-- Exercici 4.1

class Container c where
  emptyC :: c a -> Bool
  lengthC :: c a -> Int
  firstC :: c a -> a
  popC :: c a -> c a
  

-- Exercici 4.2  
  
instance Container ([]) where
  emptyC x = (null x)
  
  lengthC [] = 0
  lengthC (x:xs) = 1+(lengthC xs)
  
  firstC (x:xs) = x 

  popC (x:xs) = xs
  
-- Exercici 4.3

data Tree a = Node a [Tree a] | Empty deriving (Show)


-- Exercici 4.4

instance Container (Tree) where
  
  emptyC Empty = True
  emptyC _ = False
    
    
  firstC (Node x []) = x
  firstC (Node x _) = x
  
  
  popC (Node a []) = Empty
  popC (Node _ ((Node b nextb):ts) ) = (Node b (nextb ++ts) )
  popC (Node _ (Empty:ts) ) = (popC xs)
    where xs = (Node (firstC $ head ts) (tail ts))
          
-- No sap com tractar l'empty per tencar el bucle

    
    
-- Exercici 4.5

instance (Eq a) => Eq (Tree a) where
  Empty == Empty = True
  (Node _ _) == Empty = False
  Empty == (Node _ _) = False
  (Node x y) == (Node x2 y2) = ((x == x2) && (y == y2)) 

  
iterator :: Container c => c a -> [a]
iterator x = if (not (emptyC x)) then [firstC x]++(iterator (popC x))
                               else []
  
  
  
  
  
  
  
  
  
  
  
  
  
