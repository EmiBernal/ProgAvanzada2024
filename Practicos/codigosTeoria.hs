--Definicion de miPow(teorico)
mipow :: Int -> Int
mipow x = if x == 0 then 1 else 2*mipow(x-1)

--Definicion de miFact(teorico) usada en factorial
mifact :: Int -> Int -> Int
mifact x y = if x == 0 then y else mifact (x-1) (x*y)

factorial :: Int -> Int 
factorial n = mifact n 1

