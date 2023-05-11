{-guia 3 - Haskell-}

-- EJERCICIO 1.a
f :: Integer -> Integer
f n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16


-- EJERCICIO 1.b
g :: Integer -> Integer
g n | n == 8 = 16
    | n == 16 = 4
    | n == 131 = 1


-- EJERCICIO 1.c
   -- fog -> f(g(n))
h :: Integer -> Integer
h n = f (g n)

   -- gof -> g(f(n))
k :: Integer -> Integer 
k n = g (f n)


--EJERCICIO 2.a
absoluto :: Integer -> Integer
absoluto x | x >= 0 = x
           | otherwise = (-x)


--EJERCICIO 2.b
maxAbsolu :: Integer -> Integer -> Integer
maxAbsolu x y | (absoluto x) > (absoluto y) = x
              | (absoluto x) < (absoluto y) = y


--EJERCICIO 2.c
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y
              | z > x && z > y = z 


--EJERCICIO 2.d 
 --sin pattern matching
algunoEsCero :: Float -> Float -> Bool
algunoEsCero a b | a == 0 || b == 0 = True
                 | otherwise = False

 --con pattern matching
algunoesceroPM :: Float -> Float -> Bool
algunoesceroPM 0 _ = True
algunoesceroPM _ 0 = True
algunoesceroPM _ _ = False


--EJERCICIO 2.e
 -- sin PM
ambosSon0 :: Float -> Float -> Bool
ambosSon0 a b | a == 0 && b == 0 = True
              | otherwise = False

 -- con PM
ambosSon0' :: Float -> Float -> Bool
ambosSon0' 0 0 = True
ambosSon0' _ _ = False

--EJERCICIO 2.f
-- intervalos -> (−∞, 3],(3, 7] y (7, ∞)
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | ((x <= 3) && (y <= 3)) = True
                   | ((3 < x && x <= 7) && (3 < y && y <= 7)) = True
                   | ((7 < x) && (7 < y)) = True
                   | otherwise = False


--EJERCICIO 2.g
sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos a b c | a /= b && b /= c = a + b + c
                    | a == b && b == c = 0
                    | a == b && b /=c = c
                    | a == c && b/=c  = b
                    | b == c && a /= c = a

--EJERCICIO 2.h
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y | ((x > 0) && (y > 0)) && (mod x y == 0) = True
                 | otherwise = False


--EJERCICIO 2.i
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10


--EJERCICIO 2.j
digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100) 10


--EJERCICIO 3
-- requiere a y b /= 0
-- asegucd que es true si existe un k entero distinto de 0 tal que x*x + x*y*k = 0
-- si -(x/y) es entero true

estanRelacionados :: Int -> Int -> Bool
estanRelacionados x y = mod (-x) y == 0

--EJERCICIO 4
-- 4.a
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x1, y1) (x2, y2) = (x1*x2) + (y1*y2)


--4.b
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x1, y1) (x2, y2) = if x1 < y1 && x2 < y2 then True else False

-- o asi

todoMenor' :: (Float, Float) -> (Float, Float) -> Bool
todoMenor' (x1, y1) (x2, y2) | x1 < y1 && x2 < y2 = True
                             | otherwise = False

                        
--4.c
--con pitagoras
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1, y1) (x2, y2) = sqrt (alCuadrado((y1-y2)) + alCuadrado((x1-x2)))

--con la norma
distanciaPuntos' :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos' (x1, y1) (x2, y2) = normaVector (restaVectores ((x1, y1)) ((x2, y2)))

alCuadrado :: Float -> Float
alCuadrado x = x*x

normaVector :: (Float, Float) -> Float
normaVector (x, y) = sqrt ((x*x) + (y*y))

restaVectores :: (Float, Float) -> (Float, Float) -> (Float, Float)
restaVectores (x1, y1) (x2, y2) = (x1-x2, y1-y2)


--4.d
sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (x, y , z) = x + y + z


--4.e                                                       
sumarSoloMultiplos :: (Integer ,Integer ,Integer) -> Integer -> Integer
sumarSoloMultiplos (a, b, c) n | (esMultiploDe' n a) && (esMultiploDe' n b) && (esMultiploDe' n c) = a + b + c
                               | (esMultiploDe' n a) && (esMultiploDe' n b) = a + b
                               | (esMultiploDe' n a) && (esMultiploDe' n c) = a + c
                               | (esMultiploDe' n b) && (esMultiploDe' n c) = b + c
                               | esMultiploDe' n a = a
                               | esMultiploDe' n b = b
                               | esMultiploDe' n c = c
                               | otherwise = 0


--esta version admite enteros negativos
esMultiploDe' :: Integer -> Integer -> Bool
esMultiploDe' x y | (mod y x == 0) = True
                  | otherwise = False


--4.f
posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (x, y, z) | mod x 2 == 0 = 0
                       | mod y 2 == 0 = 1
                       | mod z 2 == 0 = 2
                       | otherwise = 4
                       

--4.g 
crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)


--4.h
invertir :: (a, b) -> (b, a) 
invertir (x, y) = (y, x)


--EJERCICIO 5
todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (x, y, z) = if (funcionF x > funcionG x) && (funcionF y > funcionG y) && (funcionF z > funcionG z) then True else False

funcionF :: Integer -> Integer 
funcionF n = if n <= 7 then n*n else (2*n) - 1 

funcionG :: Integer -> Integer
funcionG n = if mod n 2 == 0 then div n 2 else (3*n) + 1


--EJERCICIO 6
bisiesto :: Integer -> Bool
bisiesto x = if (not (esMultiploDe x 4) || (esMultiploDe x 100) && not (esMultiploDe x 400)) then False else True


--EJERCICIO 7
distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) ->  Float
distanciaManhattan (p1, p2, p3) (q1, q2, q3) = absoluto' (p1 - q1)  + absoluto' (p2 - q2) + absoluto' (p3 - q3)

--esta version admite reales
absoluto' :: Float -> Float
absoluto' x = if x >= 0 then x else (-x)


--EJERCICIO 8
comparar :: Integer -> Integer -> Integer
comparar a b | sumaUltimosDos a < sumaUltimosDos b = 1
             | sumaUltimosDos a > sumaUltimosDos b = -1
             | sumaUltimosDos a == sumaUltimosDos b = 0


sumaUltimosDos :: Integer -> Integer
sumaUltimosDos x = (mod x 10) + (div (mod x 100) 10)














