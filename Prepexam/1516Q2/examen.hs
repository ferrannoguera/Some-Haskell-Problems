-- PROBLEMA 1

allsets::a->[[a]]
allsets x = iterate (++[x]) []


-- PROBLEMA 2

alldivisors::Int->[[Int]]
alldivisors x = getdivisors x [2..x]

getdivisors::Int->[Int]->[[Int]]
getdivisors _ [] = []
getdivisors x (y:ys)
  |(mod x y) == 0 = [(quants x y)]++(getdivisors x ys)
  |otherwise = getdivisors x ys
  

quants::Int->Int->[Int]
quants 0 _ = []
quants x y
  |(mod x y) == 0 = [y]++(quants (div x y) y)
  |otherwise = []
  
  
-- PROBLEMA 3

-- Ex 3.1
data Expr a = Var String | Const a | Func String [Expr a] deriving (Show)

-- Ex 3.2
constLeafs :: Expr a -> [a]
constLeafs (Var _) = []
constLeafs (Const x) = [x]
constLeafs (Func _ y) = (tractaho y)

tractaho :: [Expr a] -> [a]
tractaho [] = []
tractaho (x:xs) = (constLeafs x)++(tractaho xs)

-- Ex 3.3
instance Functor (Expr) where
  fmap g (Var x) = Var x
  fmap g (Const x) = Const (g x)
  fmap g (Func x y) = (Func x (tractafunc g y))
  
tractafunc :: (a->b) -> [Expr a] -> [Expr b]
tractafunc _ [] = []
tractafunc g (x:xs) = [(fmap g x)]++(tractafunc g xs)


-- PROBLEMA 4 
joinx :: Eq a => [(String,a)] -> [(String,a)] -> Maybe [(String,a)]
joinx xs ys = if (null tldr) then Nothing
                             else Just (ordenaho xs ys)
    where tldr = [ x | x <- xs, y<-ys, (fst x) == (fst y), (snd x) == (snd y)]
          
ordenaho :: Eq a => [(String,a)] -> [(String,a)] -> [(String,a)]
ordenaho [] [] = []
ordenaho x [] = x
ordenaho [] y = y
ordenaho (x:xs) (y:ys)
    |(fst x) < (fst y) = [x] ++ (ordenaho xs ([y]++ys))
    |(fst x) == (fst y) = [x] ++ (ordenaho xs ys)
    |otherwise = [y] ++ (ordenaho ([x]++xs) ys)


-- PROBLEMA 5
match:: Eq a => Expr a -> Expr a -> Maybe [(String,(Expr a))]
match (Var s) (Const x) = Just [(s,(Const x))]
match (Var s) (Func _ xs) = if (length xs == 1) then Just [(s,(head xs))]
                                                else Nothing
match (Func _ [Var s]) (Const x) = Just [(s,(Const x))]
--Primers casos base per mirar que no tingui mes d'un que del altre


match (Func _ xs) (Func _ ys) = Just (assign xs ys)
-- IDEA => fer una funcio que rebi el assign i comprovi si realment compleix el match, en cas de si retorna sense els repes (join?)
--         i si no retorna nothing

assign:: Eq a => [Expr a] -> [Expr a] -> [(String,(Expr a))]
assign [] _ = []
assign ((Var s):xs) (y:ys) = [(s,y)]++(assign xs ys)



--Primer Expr -> Var segona assign
--match (Func "bin" [Var "x1",Var "x1"]) (Func "bin" [Func "un" [(Const 2)], Func "un" [(Const 2)]])

