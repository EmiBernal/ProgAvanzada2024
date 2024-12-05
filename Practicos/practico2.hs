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
milast (x:xs) = hd(reverse xs)

--Retorna toda la lista menos el ultimo elemento
miinit :: [a] -> [a]
miinit [x] = []     
miinit (x:xs) = x : miinit(xs)

{-Actividad 3: Defina una funcion maximo de tres, tal que maxTres x y z es el maximo valor entre x,y,z-}

maxTres :: Int -> Int -> Int -> Int
maxTres x y z  
    | x <= y && z <= y = y
    | y <= x && z <= x = x
    | y <= z && x <= z = z

{-Actividad 4: Definir operaciones sobre listas-}

{-Actividad 5: Defina una funcion abs: Int -> Int que calcula el valor absoluto de un numero-}

myabs :: Int -> Int
myabs x
    | x < 0 = x * (-1)
    | x >= 0 = x

{-Actividad 6: Defina una funcion edad que dada dos fechas indica los años transcurridos entre ellas-}

--edad :: (Nat, Nat, Nat) -> (Nat, Nat, Nat) -> Int
--edad x y z 

{-Actividad 7-}