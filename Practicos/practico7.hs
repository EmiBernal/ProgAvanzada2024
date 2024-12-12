import qualified Data.ByteString as 0
{-Practico Nro 7-}

{-Actividad 1 resuelta en hoja-}

{-Actividad 2-}

nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

{-Actividad 4-}

paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool) -> Bool
paraTodo xs ys p = and[p xs !! i | i <- [0..(length xs)-1]]

existe :: [Int] -> [a] -> (Int -> [a] -> Bool) -> Bool
existe xs ys p = or[p xs !! i | i <- [0..(length xs)-1]]

sumatoria :: Num a => [Int] -> [a] -> (Int -> [a] -> Bool) -> Bool
sumatoria xs ys p = sum[p xs !! i | i <- [0..(length xs)-1]]

