--Definicion de miPow(teorico)
mipow :: Int -> Int
mipow x = if x == 0 then 1 else 2*mipow(x-1)

--Definicion de miFact(teorico) usada en factorial
mifact :: Int -> Int -> Int
mifact x y = if x == 0 then y else mifact (x-1) (x*y)

factorial :: Int -> Int 
factorial n = mifact n 1

--Definicion del take

mytake :: Int -> [a] -> [a]
mytake 0 xs = []
mytake n [] = []
mytake n (x:xs) = x : mytake(n-1) xs 

--Definicion de drop

mydrop :: Int -> [a] -> [a]
mydrop 0 xs = xs
mydrop _ [] = []
mydrop n (x:xs) = drop (n-1) xs

--Definicion de suma para ver los patrones de recursion

mysum :: [Int] -> Int 
mysum [] = 0
mysum (x:xs) = x + mysum xs

--Definiciones de foldr y foldl

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f z [] = z  --Z es el elemento que devolvemos en caso de tener una lista vacia
myfoldr f z (x:xs) = f x (myfoldr f z xs)

myfoldl :: (a-> b -> a) -> a -> [b] -> a
myfoldl f z [] = z
myfoldl f z (x:xs) = myfoldl f (f z x) xs

myfoldl' :: (a -> b -> a) -> a -> [b] -> a
myfoldl' f z [] = z
myfoldl' f z (x:xs) = let z' = z `f` x
                      in seq z' (myfoldl' f z' xs)

--Caso base: Si la lista esta vacia, devuelvo el neutro de la operacion
--Caso recursivo: Cuando uso el let defino una "variable" z', en donde, aplicamos la funcion f al acumulador z y a al primer elemento de la lista. Para luego, en in, fuerzo la evaluacion del z' para luego ser llamado nuevamente recursivamente con el resto de la lista

--Definicion de map

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x:xs)
    | f x = x : myfilter f xs
    | otherwise = myfilter f xs

isEven :: Integral a => a -> Bool
isEven a = if a `mod` 2 == 0 then True else False

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
