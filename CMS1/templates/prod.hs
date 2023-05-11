-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(prod(x ::(Integer)))
  }

prod :: Integer -> Integer 
-- Completar la definición de la función
prod n = prodAux 1 (2*n)
 where prodAux :: Integer -> Integer -> Integer
       prodAux i j | i > j = 1
                   | otherwise = (i^2 + 2*i) * prodAux (i+1) j
-- Pueden agregan las funciones que consideren necesarias


