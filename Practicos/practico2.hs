{--Practico Nro 2--}

-- Ejercicio 2 
{-Definir las siguientes funciones-}

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

-- Ejercicio 3
{-Defina una funcion maximo de tres, tal que maxTres x y z es el maximo
valor entre x, y, z . Por ejemplo: maxTres 6 7 4 = 7.-}

maxTres :: Int -> Int -> Int -> Int
maxTres a b c = max a (max b c)

-- Ejercicio 4
{-Defina las siguientes operaciones sobre listas (vistas en el te ́orico): concate-
nar, tomar(take) y tirar)-}

tomar :: Int -> [a] -> [a]
tomar 0 _ = []
tomar _ [] = []
tomar n (x:xs)
  | n > 0     = x : tomar (n-1) xs
  | otherwise = []

concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys

tirar :: Int -> [a] -> [a]
tirar n [] = []
tirar 0 xs = xs
tirar n (x:xs) = drop (n-1) xs

-- Ejercicio 5
{-Defina una funcion abs: Int -> Int que calcula el valor absoluto de un
numero.-}

abss :: Int -> Int
abss x
  | x >= 0    = x
  | otherwise = -x

-- Ejercicio 6
{-Defina una funcion edad :: (Nat,Nat,Nat) -> (Nat,Nat,Nat) -> Int
que dada dos fechas indica los años transcurridos entre ellas.-}

edad :: (Int, Int, Int) -> (Int, Int, Int) -> Int
edad (x,y,z) (p,q,r) | (q<y) || (q==y && p<x) = r - z - 1 
               | otherwise = r - z 

--Ejercicio 7
{-La disyuncion excluyente xor de dos formulas se verifica si una es verdadera
y la otra es falsa. Defina la funcion xor que calcule la disyuncion excluyente a
partir de la tabla de verdad.-}

xor :: Bool -> Bool -> Bool 
xor True True = False
xor False False = False
xor True False = True
xor False True = True

{-Ahora defina la funcin xor2 que calcule la disyuncion excluyente pero sin
que considere todos los posibles valores de las entradas.-}

xor2 :: Bool -> Bool -> Bool
xor2 p q | (p==q) = False | otherwise = True

xor3 :: Bool -> Bool -> Bool 
xor3 True True = False
xor3 False False = False
xor3 _ _ = True             --(_ _) son los comodines, en este caso si es true false o false true va a devolver true.

-- Ejercicio 8
{-Defina una funcion que dado un numero natural, decida si el mismo es primo
o no.-}

prim :: Int -> Bool
prim n = length(division n) == 2

division :: Int -> [Int] -- funcion auxiliar
division n = [x | x <- [1..n], n `mod` x == 0] {-x tal que x pertenece a los N tal que mod n x == 0-}

-- Ejercicio 9
{-Defina una funcion que dado un numero natural n, retorne la lista de todos
los numeros naturales primos menores que n.-}

divisionPrim :: Int -> [Int] -- funcion auxiliar
divisionPrim n = [x | x <- [1..n-1], prim x] {-x tal que x pertenece a los N tal que cumple la condicion que prim se cumpla de x-}

-- Ejercicio 10
{-Defina una funcion que dada una lista, retorne la reversa de la misma.-}

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

-- Ejercicio 11
{-Defina una lista de numeros, devuelva la lista solo con los numeros primos.-}

listaNum ::[Int] -> [Int]
listaNum [] = []
listaNum n = [x | x <- n, prim x]

-- Ejercicio 12
{-Defina una funcion que dada una lista decida si es un palindromo o no.-}

palindromo :: (Eq a) => [a] -> Bool
palindromo xs = xs == reverse xs

-- Ejercicio 13
{-Defina una funcion que dados tres n ́umeros a, b, c devuelva la cantidad de
raices reales de la ecuacion ax2 + bx + c = 0-}

discriminante :: Float -> Float -> Float -> Float 
discriminante a b c = (b^2) - 4*a*c

cantidadRaicesReales :: Float -> Float -> Float -> Int
cantidadRaicesReales a b c
  | disc > 0  = 2
  | disc == 0 = 1
  | otherwise = 0
  where
    disc = discriminante a b c



