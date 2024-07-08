{--Practico Nro 2--}

{-2-}
hd::[a]->a --Selecciona y muestra el primer elemento de la lista
hd (x:xs) = x   --x:xs son los parametros


tl::[a]->[a] --Selecciona y muestra el resto de elementos de la lista menos el primer elemento
tl (x:xs) = xs

lasRever :: [a] -> a --Retorna el ultimo elemento de la lista
lasRever (x:xs) = hd(reverse xs)

las :: [a] -> a --retorna el ultimo elemento de la lista de forma recursiva
las [x] = x
las (x:xs) = las xs

ini :: [a] -> [a] --retorna toda la lista menos el ultimo elemento de la lista
ini [x] = [] --caso base
ini (x:xs) =  x : ini xs--caso inductivo


{-3-}

maxTres :: Int -> Int -> Int -> Int
maxTres a b c = max a (max b c)

{-4 hacer en casa

concaten :: [a] -> [a]
concaten [a] 
-}

{-5-}

ab :: Int -> Int
ab x | x >= 0 = x   
     | x < 0 = (-x)

{-6-}

edad :: (Int, Int, Int) -> (Int, Int, Int) -> Int

edad (x,y,z) (p,q,r) | (q<y) || (q==y && p<x) = r - z - 1 
               | otherwise = r - z 

{-Si los años son iguales, tiene 0 años
Si los años son distintos se hace la resta de los años y :   
Si los años son distintos y el segundo mes es mas grande es pq ya los cumplio:
Si los dias son distintos y el segundo dia es mas grande o igual, entonces le sumo 1 a la resta de los años
Si los dias son distintos y el segundo dia es mas chico, entonces devuelvo la resta de los años
Si los años son distintos y el segundo mes es mas chico es pq todavia no los cumplio:
Devuelvo la resta de los años
-}

{-7-}

xor :: Bool -> Bool -> Bool 
xor True True = False
xor False False = False
xor True False = True
xor False True = True

xor2 :: Bool -> Bool -> Bool
xor2 p q | (p==q) = False | otherwise = True

xor3 :: Bool -> Bool -> Bool 
xor3 True True = False
xor3 False False = False
xor3 _ _ = True             --(_ _) son los comodines, en este caso si es true false o false true va a devolver true.

{-8-}

prim :: Int -> Bool
prim n = length(division n) == 2

division :: Int -> [Int] -- funcion auxiliar
division n = [x | x <- [1..n], n `mod` x == 0] {-x tal que x pertenece a los N tal que mod n x == 0-}

{-9-}

divisionPrim :: Int -> [Int] -- funcion auxiliar
divisionPrim n = [x | x <- [1..n-1], prim x] {-x tal que x pertenece a los N tal que cumple la condicion que prim se cumpla de x-}

{-10-}

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

{-
[2,3] ++ [1]
[3] ++ [2] ++ [1]
[] ++ [3] ++ [2] ++ [1]
-}

{-11-}

listaNum ::[Int] -> [Int]
listaNum [] = []
listaNum n = [x | x <- n, prim x]

{-12-}

palindromo :: (Eq a) => [a] -> Bool
palindromo xs = xs == reverse xs

{-13-}

discriminante :: Float -> Float -> Float -> Float 
discriminante a b c = (b^2) - 4*a*c


