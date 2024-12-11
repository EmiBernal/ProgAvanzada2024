data Nat = Zero | Succ Nat
    deriving(Eq, Show)

instance Num Nat where

  -- Adición
  Zero + n = n
  (Succ m) + n = Succ (m + n)

  -- Resta
  n - Zero = n
  Zero - _ = error "Estamos en los números naturales, no están permitidos los negativos"
  (Succ m) - (Succ n) = m - n

  -- Multiplicación
  Zero * _ = Zero
  (Succ m) * n = n + (m * n)

  -- Negación
  negate _ = error "Estamos en los números naturales, no están permitidos los negativos"

  -- Valor absoluto
  abs n = n

  -- Signo
  signum Zero = Zero
  signum (Succ _) = Succ Zero

  -- Conversión desde Integer
  fromInteger 0 = Zero
  fromInteger n
    | n > 0     = Succ (fromInteger (n - 1))
    | otherwise = error "Estamos en los números naturales, no están permitidos los negativos"
