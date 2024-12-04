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

--Definicion de miPow(teorico)
mipow :: Int -> Int
mipow x = if x == 0 then 1 else 2*mipow(x-1)