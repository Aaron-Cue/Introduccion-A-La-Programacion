-- No editar esta parte
main :: IO ()
main = do
  x <- readLn
  print (sumaMenosQueMax (x :: (Int, Int, Int)))

sumaMenosQueMax :: (Int, Int, Int) -> Bool
-- Completar acá la definición de la función
sumaMenosQueMax (x, y, z) = if (maxim (x, y, z)) > ((minim (x, y, z)) + (medio (x, y, z))) then True else False
-- Pueden agregan las funciones que consideren necesarias


maxim :: (Int, Int, Int) -> Int
maxim (x, y, z)
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | z >= x && z >= y = z
    | otherwise = error "argumento no válido"

minim :: (Int, Int, Int) -> Int
minim (x, y, z)
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | z <= x && z <= y = z
    | otherwise = error "argumento no válido"

medio :: (Int, Int, Int) -> Int
medio (x, y, z) | x == y && y == z = x
                | otherwise = max (min x y) (min x z) `max` min y z


















