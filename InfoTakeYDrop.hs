{-Las funciones take y drop son funciones utiles en Haskell para trabajar con listas

La funcion `take` toma un numero entero `n` y una list, y devuelve los primeros `n` elementos de la lista. Si la lista tiene menos de `n` elementos devuelve la lista entera 

take 3 [1, 2, 3, 4, 5] -- Devuelve [1, 2, 3]
take 10 [1, 2, 3] -- Devuelve [1, 2, 3], ya que la lista original tiene menos de 10 elementos

{-La funcion drop toma un numero entero `n` y una lista, y elimina los primeros `n` elementos de esa lista. Si la lista tiene menos de n elementos, devuelve una lista vacía.  

drop 3 [1, 2, 3, 4, 5] -- Devuelve [4, 5]
drop 10 [1, 2, 3] -- Devuelve [], ya que la lista original tiene menos de 10 elementos
-}
