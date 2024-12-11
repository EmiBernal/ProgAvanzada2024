{-Practico Nro 5-}

{-Actividad 1-}
lisInf 1 = 1 : lisInf 1

{-Actividad 2-}

lisInf n = n : lisInf (n+1)

{-Actividad 3-}

primNumeros :: Int -> [Int]
primNumeros n = [0..n-1]

{-Actividad 4-}

lisInf5 :: Int -> [Int]
lisInf5 n = take 5 [n..]

{-Actividad 5-}

cuadrados :: [Int] -> [Int]
cuadrados = map (^2)

{-Actividad 6-}

divisore :: Int -> [Int]
divisore n = let x = [1..n]
             in filter (\x -> n `mod` x == 0) x

{-Actividad 7-}

esPrimo :: Int -> Bool
esPrimo n
    | n < 2 = False
    | otherwise = null [x | x <- [2..n-1], n `mod` x == 0]

listaPrimos :: [Int] -> [Int]
listaPrimos xs = filter esPrimo xs
 
{-Actividad 8-}

sumaCuadrados :: [Int] -> Int
sumaCuadrados [] = 0
sumaCuadrados xs = sum (map (^2) xs)

{-Actividad 9-}

listaSucesores :: [Int] -> [Int]
listaSucesores xs = map (+1) xs

{-Actividad 10-}

sumaLista :: [Int] -> Int
sumaLista xs = foldr (+) 0 xs

{-Actividad 11-}

factorial :: Int -> Int
factorial 0 = 0
factorial 1 = 0
factorial n = foldr (*) 1 (list n)

list :: Int -> [Int]
list 0 = []
list 1 = [1]
list n = n : list (n-1)

{-Actividad 12-}

myand :: [Bool] -> Bool
myand [] = False
myand xs = foldr (&&) True xs

{-Actividad 13-}

tam :: [a] -> Int
tam [] = 0
tam [x] = 1
tam (x:xs) = foldr (const (+ 1)) 0 xs

-- const (+ 1) es una función que siempre devuelve (+ 1), independientemente de su argumento.

taml :: [a] -> Int
taml [] = 0
taml [x] = 1
taml (x:xs) = foldl (\acc _ -> acc + 1 ) 0 xs

-- _ funciona como un placeholder para cada elemento de la lista, pero no le interesa cual es el elemento. Mientras haya elemento incrementa el acumulador
-- El 0 funciona como el valor INICIAL del acumulador

{-Actividad 14-}

sucesoresLista :: [Int] -> [Int]
sucesoresLista xs = [x+1 | x <- xs]

{-Actividad 15-}

cuadradosLista :: [Int] -> [Int]
cuadradosLista xs = [x^2 | x <- xs]

{-Actividad 16-}

paresMayoresDiez :: [Int] -> [Int]
paresMayoresDiez xs = [x | x <- xs, esPrimo x && x>10]

{-Actividad 17-}

divisoresLista :: Int -> [Int]
divisoresLista n = [x | x <- [1..n], n `mod` x == 0]

{-Actividad 18-}

--todosOcurrenEn :: (Eq a) => [a] -> [a] -> Bool
--todosOcurrenEn xs ys =  null [x| x <- xs,  x `notElem` ys] 

--Null verifica si la lista esta vacia, si fuera ese el caso, devuelve true

{-Actividad 19-}

numerosPrimos :: Int -> [Int]
numerosPrimos n = [x | x <- [2..n], x `mod` 2 == 0]

{-Actividad 20-}

productoCart :: [Int] -> [Int] -> [(Int,Int)]
productoCart xs ys = [(x,y)| x <- xs, y <- ys]

{-Actividad 21-}

ocurrencias :: (Eq a) => [a] -> a -> Int
ocurrencias ys y = length[x | x <- ys, x == y]

{-Actividad 22-}

split2 :: [a] -> [([a],[a])]
split2 xs = [(take i xs, drop i xs) | i <- [0..length xs]]

{-Actividad 23-}

segmenInit :: [a] -> [[a]]
segmenInit xs = [(take i xs) | i <- [0..length xs]]

sumaSegmentInit :: [Int] -> Int
sumaSegmentInit xs = foldr (+) 0 (map sum (segmenInit xs))

{-Actividad 24-}

lisInfPares = [x | x <- [0..], even x]