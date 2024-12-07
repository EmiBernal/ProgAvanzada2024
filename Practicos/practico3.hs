{-Practico Nro 3-}

{-Actividad 1-}

mymerge :: [Int] -> [Int] -> [Int]
mymerge [] [] = []
mymerge (x:xs) (y:ys) = x : y : mymerge xs ys 

{-Actividad 2-}

ordena :: [Int] -> [Int]
ordena [] = []
ordena [x] = [x]
ordena (x:y:xs) 
    | x > y = y : ordena (x:xs)
    | otherwise = x : ordena(y:xs)

{-Actividad 3-}

base2 :: Int -> Int
base2 0 = 1
base2 n = 2 * base2(n-1)

{-Actividad 4-}

calculobinario :: Int -> [Int]
calculobinario 0 = [0]
calculobinario n = (n `mod` 2) : calculobinario(n `div` 2)

binario :: Int -> [Int]
binario n = reverse(calculobinario n)

{-Actividad 5-}

binariopar :: [Int] -> Bool
binariopar xs = last xs == 0

{-Actividad 6-}

distanciaH :: [Char] -> [Char] -> Int
distanciaH [] [] = 0
distanciaH (x:xs) (y:ys) 
    | x == y = distanciaH xs ys 
    | otherwise = 1 + distanciaH xs ys

{-Actividad 7-}

esentero :: Float -> Bool
esentero n = (n - fromIntegral(floor n)) == 0

cuadradoperfecto :: Int -> Bool
cuadradoperfecto n = esentero(sqrt(fromIntegral n))  

{-Actividad 8-}

repetidos :: a -> Int -> [a]
repetidos a 0 = []
repetidos a n = a : repetidos a (n-1)

{-Actividad 9-}

nelem :: [a] -> Int -> a
nelem (x:xs) 0 = x
nelem (x:xs) n = nelem xs (n-1)

{-Actividad 10-}

posicionesC :: Eq a => [a] -> a -> [Int]
posicionesC xs y = posicionesAux xs y 0

posicionesAux :: Eq a => [a] -> a -> Int -> [Int]
posicionesAux [] _ _ = []
posicionesAux (x:xs) y n
    | x == y = n : posicionesAux xs y (n+1)
    | otherwise = posicionesAux xs y (n+1)

{-Actividad 11-}

compact :: (Eq a) => [a] -> [a]
compact [] = []
compact [x] = [x]
compact (x:y:xs)
    | x == y = compact (x:xs)
    | otherwise = x : compact(y:xs)