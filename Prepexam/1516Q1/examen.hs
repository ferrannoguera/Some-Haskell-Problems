-- Problema 1

-- Ex 1.1
quadrats::[Integer]
quadrats = [x*x | x<-[1..]]

-- Ex 1.2
sumQuadrats::Integer->[Integer]
sumQuadrats x = takeWhile (<=x) quadrats

-- fer dos funcions, aquesta primera tindra aquests dos parametress i els rebra una altre funcio que anira recorrent tota la llista fent sumes fins que es passi del tamany del x o s'acabi la llista, si recorre tota la llista = false else = true


