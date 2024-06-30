{-Practico 1-}

--Ejercicio 3-- 
{-Utilizando las funciones hd y tail, y dada la lista “hola mundo”, obtenga
el segundo elemento de la misma (la letra “o”).-}

-- head( tail ['h','o','l','a','m','u','n','d','o']) --Escribiendo esto tal cual en la terminal

--Ejercicio 4

{-Utilizando las funciones hd y reverse, y dada la lista “hola mundo”,
obtenga el ultimo elemento de la misma (la letra “o”).-}

--head (reverse ['h','o','l','a','m','u','n','d','o']) --Escribiendo esto tal cual en la terminal

--Ejercicio 5

{-Utilizando la funcion realizada en el ejercicio anterior y la funcion mod
determine si un numero, representado como la lista de sus dıgitos (ej: 123
= [1,2,3]) es par.-}

--(head (reverse [1,2,3]) `mod` 2) == 0 --Escribiendo esto tal cual en la terminal

--Ejercicio 6

{-Utilizando la funcion sum1, la funcion mod y un numero representado de
igual manera que en el [ item 5 ] determine si un numero es multiplo de 3.-}

--((sum [1,2,3] `mod` 3) == 0 )  --Suponiendo que queremos saber si el 6 es multiplo de 3 ya que [1,2,3] = 1 + 2 + 3 = 6 mod 3 =? 0 

--Ejercicio 7

{-Utilizando las funciones de los [ items 5, 6 ] determine si un numero es
multiplo de 6-}

--((((head(reverse a) `mod` 2)) && (sum(a) `mod` 3) == 0)) --Escribiendo esto tal cual en la terminal

--Ejercicio 8

{-Escriba una función que dado un número retorne la lista de sus digitos-}

digitos :: Int -> [Int]
digitos n
        | n < 10 = [n]
        | otherwise = digitos (n `div` 10) ++ [n `mod` 10]

--Si el numero ingresado es menor a 10, se devuelve una lista que ese numero. En cambio, si el numero es mayor a 10, se divide el numero por 10 para eliminar el ultimo digito y se llama recursivamente a la funcion "digitos" con el numero restante, mientras se agrega el ultimo digito a la lista con el operador concatenacion. 

--Ejercicio 9

{-Investigue las definiciones de las funciones take y drop, utilizando esta
funciones implemente una funcion cortar :: Int -> Int -> [Char]
->[Char] que dados dos enteros i y j y un string w, devuelva el substring
que se encuentra entre las posiciones i y j.-}

cortar :: Int -> Int -> [Char] -> [Char]
cortar a b s= take (b - a) (drop (a - 1) s)

{-b - a = Es la longitud del subintervalo del intervalo mas grande (que en este caso es una palabra). Solo con el take ya estaria. Pero si le agregamos el drop: drop (a - 1) s elimina los primeros a - 1 elementos de la cadena s-}

--Ejercicio 10

{-¿Que arrojar´a como resultado la evaluaci´on de la siguiente expresi´on en
ghci?

(head.(drop 3)) "0123456"-}

{-La expresión head . (drop 3) se compone de dos funciones en Haskell que se combinan utilizando el operador de composición (.). La función head se aplica al resultado de la función drop 3.

La función drop 3 toma una lista y elimina los primeros 3 elementos de esa lista.
La función head toma una lista y devuelve su primer elemento.
Por lo tanto, head . (drop 3) eliminará los primeros 3 elementos de la lista y luego devolverá el primer elemento que queda.

Veamos cómo funciona con el argumento "0123456":

drop 3 "0123456" elimina los primeros 3 caracteres, resultando en "3456".
head "3456" toma el primer carácter de esa cadena, que es '3'.
Por lo tanto, el resultado de la expresión head . (drop 3) "0123456" es '3'.-}

--(head.(drop 3)) "0123456" --Devuelve 3 

--El tipo de valor es numerico

{-En GHCi (el intérprete de Haskell), puedes usar el comando :step para ver el número de pasos realizados por el intérprete durante la evaluación de una expresión. Este comando te permite realizar la evaluación paso a paso, mostrando cada paso y el estado del intérprete después de ese paso.-}