-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(sumaPrimerosNImpares(x ::(Integer)))
  }

sumaPrimerosNImpares :: Integer -> Integer
-- Completar la definición de la función
sumaPrimerosNImpares n = sumatoriaImparAux 1 ((2*n) - 1)
-- Pueden agregan las funciones que consideren necesarias
sumatoriaImparAux :: Integer -> Integer -> Integer
sumatoriaImparAux i j | i > j = 0
                      | mod i 2 == 0 = sumatoriaImparAux (i+1) j 
                      | otherwise = (2*i + 2) + sumatoriaImparAux (i+2) j




