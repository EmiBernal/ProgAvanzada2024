{-Practico Nro 2-}

{-Actividad 1: Leer los capitulos 1 y 2 del libro Aprende Haskell-}

{-Actividad 2: Define las siguientes funciones-}

--Retorna el primer elemento de una lista
hd :: [a] -> a
hd (x:xs) = x

--Retorna toda la lista menos el primer elemento
tl :: [a] -> [a]
tl (x:xs) = xs

--Retorna el ultimo elemento de la lista
milast :: [a] -> a
milast (x:xs) = hd (reverse xs)

--Retorna toda la lista menos el ultimo elemento
miinit :: [a] -> [a]
miinit [x] = []
miinit (x:xs) = x : miinit (xs)

{-Actividad 3: Defina una funcion maximo de tres, tal que maxTres x y z es el maximo valor entre x,y,z-}

maxTres :: Int -> Int -> Int -> Int
maxTres x y z
    | x <= y && z <= y = y
    | y <= x && z <= x = x
    | y <= z && x <= z = z

{-Actividad 4: Definir operaciones sobre listas-}

concatenar :: [a] -> [a] -> [a]
concatenar [] [] = []
concatenar [x] [] = [x]
concatenar [] [y] = [y]
concatenar (x:xs) ys = x : concatenar xs ys

--take
tomar :: Int -> [a] -> [a]
tomar _ [] = []
tomar 0 _ = []
tomar n (x:xs)
    | n > 0 = x : tomar (n-1) xs
    | otherwise = []

--drop
tirar :: Int -> [a] -> [a]
tirar _ [] = []
tirar 0 xs = xs
tirar n (x:xs) = tirar (n-1) xs

--Agregar por derecha
agregar :: a -> [a] -> [a]
agregar n [] = [n]
agregar n (x:xs) = x : agregar n xs

{-Actividad 5: Defina una funcion abs: Int -> Int que calcula el valor absoluto de un numero-}

myabs :: Int -> Int
myabs x
    | x < 0 = x * (-1)
    | x >= 0 = x

{-Actividad 6: Defina una funcion edad que dada dos fechas indica los años transcurridos entre ellas-}

edad :: (Int, Int, Int) -> (Int, Int, Int) -> Int
edad (a, b, c) (x, y, z)
    | a > x || (a == x && b > y) = error "Fecha de nacimiento posterior a la fecha actual"
    | a == x && b == y = x - z
    | a >= x = (x - z) - 1

{-Actividad 7-}

myxor :: Bool -> Bool -> Bool
myxor True True = False
myxor True False = True
myxor False True = True
myxor False False = False

myxor2 :: Bool -> Bool -> Bool
myxor2 p q
    | p == q = False
    | otherwise = True

{-Actividad 8-}

esprimo :: Int -> Bool
esprimo n = length (listaDivisores n) == 2

listaDivisores :: Int -> [Int]
listaDivisores n = [x | x <- [1..n], n `mod` x == 0]

{-Actividad 9-}

{-Actividad 10-}

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

{-Actividad 11-}

soloprimos :: [Int] -> [Int]
soloprimos xs = [x | x <- xs, esprimo x]

{-Actividad 12-}

palindromo :: (Eq a) => [a] -> Bool
palindromo xs = xs == reverse xs

{-Actividad 13-}