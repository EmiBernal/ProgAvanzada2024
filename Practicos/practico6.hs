import Prelude hiding ((<=), (/=), (==))
import Data.Functor.Classes (eq1)
{-Practico Nro 6-}

{-Actividad 1-}
data Nat = Zero | Succ Nat

{-Actividad 2-}
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ m) = 1 + natToInt m

{-Actividad 3-}
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n 
    | n > 0 = Succ(intToNat(n-1))
    | n < 0 = error "Los numeros naturales no aceptan negativos"

{-Actividad 4-}
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero Zero = Zero
sumaNat Zero n = n
sumaNat n Zero = n
sumaNat (Succ n) m = Succ(sumaNat n m)

{-Actividad 5-}
instance Eq Nat where

-- Igualdad
Zero == Zero = True
Zero == _    = False
_ == Zero    = False
(Succ m) == (Succ n) = m == n

-- Desigualdad
Zero /= Zero = False
Zero /= _ = True
_ /= Zero = True
(Succ n) /= (Succ m) = n /= m

instance Ord Nat where

--Menor igual
Zero <= n = True
n <= Zero = False
(Succ n) <= (Succ m) = n <= m

instance Show Nat where
    show Zero = "0"
    show (Succ n) = show n ++ " + 1"

{-Actividad 6-}
data Tree a = Nil | Node (Tree a) a (Tree a)

{-Actividad 7-}
size :: Tree a -> Int
size Nil = 0
size (Node hi r hd) = 1 + size hi + size hd

{-Actividad 8-}
height :: Tree a -> Int
height Nil = 0
height (Node hi r hd) = 1 + max (height hi)(height hd)

{-Actividad 9-}
instance Eq a => Eq (Tree a) where

--Igualdad
Nil == Nil = True
_ == Nil = False
Nil == _ = False
(Node Nil a Nil) == (Node Nil b Nil) = a == b
(Node hi a hd) == (Node hi' b hd') 
                        | a == b = (hi == hi') && (hd == hd')
                        | otherwise = False

--Al tener que instanciar el ord en arboles binarios, estamos ante una estructura muy abstracta la cual puede instanciarse de varias maneras, por lo que es necesario especificar de que manera se desea la instancia. Si en base a los nodos, cantidad de nodos, primer elemento, altura, etc. Como estamos hablando de arboles binarios, lo mas comum es ver si esta bien ordenado por nodos. Ya que los nodos mas a la izquierda de la raiz deben ser menor al mismo, mientras que los mayores a la raiz estan en la parte derecha del mismo.

instance Ord a => Ord (Tree a) where
    
Nil <= _ = True
_ <= Nil = False
(Node left1 x1 right1) <= (Node left2 x2 right2) 
                | x1 < x2 = True 
                | x1 == x2 = left1 <= left2 && right1 <= right2 
                | otherwise = False

instance Show (Tree a) where
    Show nil = "<>"
    Show (Node Nil r Nil) = "<" ++ show r ++ ">"
    Show (Node hi r hd) = "<" ++ show hi ++ "," ++ show r ++ "," ++ show hd ++ ">" 