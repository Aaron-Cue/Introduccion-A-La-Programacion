-- No editar esta parte
main :: IO()
main = do {
  x <- readLn ;
  print(combinacionesMenoresOiguales(x ::(Integer)))
  }


combinacionesMenoresOiguales :: Integer -> Integer
-- Completar la definición de la función
combinacionesMenoresOiguales n = aux1 1
  where
    aux1 :: Integer -> Integer
    aux1 i
      | i > n     = 0
      | otherwise = aux2 i 1 + aux1 (i + 1)

    aux2 :: Integer -> Integer -> Integer
    aux2 i j
      | j > n            = 0
      | i * j <= n       = 1 + aux2 i (j + 1)
      | otherwise        = aux2 i (j + 1)
-- Pueden agregan las funciones que consideren necesarias