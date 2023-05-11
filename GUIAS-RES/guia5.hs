--EJERCICIO 1.1
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


--EJERCICIO 1.2
ultimo :: [t] -> t
ultimo xs = xs !! ((longitud xs) - 1)



--EJERCICIO 1.3 
principio :: [t] -> [t]   
principio [_] = []
principio (x:xs) = x : principio xs



--EJERCICIO 1.4
reverso :: (Eq t) => [t] -> [t] 
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]



--EJERCICIO 2.1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece y (x:xs) | y == x = True
                   | otherwise = pertenece y xs



--EJERCICIO 2.2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [_] = True 
todosIguales (x:y:xs) = x == y && todosIguales (y:xs)



--EJERCICIO 2.3
todosDistintos :: Eq a => [a] -> Bool
todosDistintos [] = True
todosDistintos (x:xs) = not (pertenece x xs) && todosDistintos xs



--EJERCICIO 2.4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs



--EJERCICIO 2.5
quitar :: (Eq t) => t -> [t] -> [t]
quitar y [] = []
quitar y (x:xs) | y /= x && not (pertenece y xs) = (x:xs)
                | y == x = xs
                | otherwise = x : quitar y xs


--EJERCICIO 2.6
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos y [] = []
quitarTodos y (x:xs) | y /= x && not (pertenece y xs) = (x:xs)
                     | y == x && not (pertenece y xs) = xs
                     | y /= x && pertenece y xs = x:quitarTodos y xs
                     | y == x && pertenece y xs = quitarTodos y xs



--EJERCICIO 2.7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) | hayRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)
                         | otherwise = x : eliminarRepetidos xs



--EJERCICIO 2.8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos [] _ = False
mismosElementos _ [] = False
mismosElementos (x:xs) ys = if pertenece x ys then mismosElementos xs (quitar x ys) else False



--EJERCICIO 2.9
capicua :: (Eq t) => [t] -> Bool
capicua l = if l == reverso l then True else False




--EJERCICIO 3.1
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs 



--EJERCICIO 3.2
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs



--EJERCICIO 3.3
maximo :: [Int] -> Int
maximo [x] = x
maximo (x:xs) = if x > maximo xs then x else maximo xs

--extra 
minimo :: [Int] -> Int
minimo [x] = x
minimo (x:xs) = if x < minimo xs then x else minimo xs


--EJERCICIO 3.4
sumarN :: Int -> [Int] -> [Int]
sumarN _ [] = []
sumarN n [x] = [x+n]
sumarN n (x:xs) = (x+n) : sumarN n xs



--EJERCICIO 3.5
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarElPrimeroAux x (x:xs)
  where
    sumarElPrimeroAux _ [] = []
    sumarElPrimeroAux n (y:ys) = (n + y) : sumarElPrimeroAux n ys


--EJERCICIO 3.6
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo [] = []
sumarElUltimo [x] = [x+x]
sumarElUltimo (x:xs) = ((ultimo xs) + x):sumarElUltimo xs



--EJERCICIO 3.7
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | esPar x = x:pares xs
             | otherwise = pares xs
             where esPar n = if mod n 2 == 0 then True else False



--EJERCICIO 3.8
multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n [] = []
multiplosDeN n (x:xs) = if esMultiploDe n x then x : (multiplosDeN n xs) else multiplosDeN n xs

-- aux 3.8
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | ((x > 0) && (y > 0)) && (mod x y == 0) = True
                 | otherwise = False


--EJERCICIO 3.9
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar (x:xs) = ordenarAux x (ordenar xs)
  where
    ordenarAux x [] = [x]
    ordenarAux x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : ordenarAux x ys



--EJERCICIO 4.1
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x1:x2:xs) | x1 == ' ' && x2 == ' ' = sacarBlancosRepetidos (x2:xs)
                                 | otherwise = x1:sacarBlancosRepetidos (x2:xs)


--EJERCICIO 4.2
contarPalabras :: [Char] -> Int
contarPalabras [] = 0 
contarPalabras xs = (cantidadEspacios (sacarBlancosRepetidos xs)) + 1

--aux
cantidadEspacios :: [Char] -> Int
cantidadEspacios [] = 0
cantidadEspacios (x:xs) = if x == ' ' then 1 + cantidadEspacios xs else cantidadEspacios xs


--EJERCICIO 4.3
palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga [x] = [x]
palabraMasLarga (x:xs) | longitud palabraActual > longitud palabraMasLargaTail = palabraActual
                       | otherwise = palabraMasLargaTail
                       where palabraActual = tomarPalabra (x:xs) 
                             palabraMasLargaTail = palabraMasLarga xs 

--aux
tomarPalabra :: [Char] -> [Char]
tomarPalabra [] = [] 
tomarPalabra (x:xs)
  | x == ' ' = [] 
  | otherwise = x : tomarPalabra xs



--EJERCICIO 4.4    --casi bien
palabras :: [Char] -> [[Char]]
palabras [] = [[]]
palabras (x:xs) = [tomarPalabra (x:xs)] ++ palabras (restoPalabras (x:xs))


restoPalabras :: [Char] -> [Char]
restoPalabras [] = []
restoPalabras (y:ys)
  | y == ' ' = ys
  | otherwise = restoPalabras ys


--EJERCICIO 4.5
aplanar :: [[Char]] -> [Char]
aplanar [[]] = []
aplanar [x] = x
aplanar (x:xs) = x ++ aplanar xs


--EJERCICIO 4.6
aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos [[]] = []
aplanarConBlancos [x] = x
aplanarConBlancos (x:xs) = x ++ " " ++ aplanarConBlancos xs


--EJERCICIO 4.7  -- n >= 0
aplanarConNBlancos :: [[Char]] -> Int -> [Char]
aplanarConNBlancos [[]] n = []
aplanarConNBlancos [x] n = x
aplanarConNBlancos (x:xs) n = x ++ nEspacios n ++ aplanarConNBlancos xs n

nEspacios :: Int -> [Char]
nEspacios 0 = ""
nEspacios n = " " ++ nEspacios(n - 1)



--EJERCICIO 5.1    -- n >= 0
nat2bin :: Int -> [Int]
nat2bin 0 = [0]
nat2bin 1 = [1]
nat2bin n = nat2bin (div n 2) ++ [mod n 2] 



--EJERCICIO 5.2
bin2nat :: [Int] -> Int
bin2nat xs = pasarAEntero (reverso xs) 0

pasarAEntero :: [Int] -> Int -> Int
pasarAEntero [] _ = 0
pasarAEntero (x:xs) exp = if x == 1 then 2^exp + pasarAEntero xs (exp + 1) else 0 + pasarAEntero xs (exp + 1)



--EJERCICIO 5.3
nat2hex :: Int -> [Char]
nat2hex 0 = ['0']
nat2hex 1 = ['1']
nat2hex 2 = ['2']
nat2hex 3 = ['3']
nat2hex 4 = ['4']
nat2hex 5 = ['5']
nat2hex 6 = ['6']
nat2hex 7 = ['7']
nat2hex 8 = ['8']
nat2hex 9 = ['9']
nat2hex 10 = ['A']
nat2hex 11 = ['B']
nat2hex 12 = ['C']
nat2hex 13 = ['D']
nat2hex 14 = ['E']
nat2hex 15 = ['F']
nat2hex n = nat2hex (div n 16) ++ nat2hex (mod n 16)


--EJERCICIO 5.4
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada xs = sumaAcumuladaAux xs 0

sumaAcumuladaAux :: (Num t) => [t] -> t -> [t]
sumaAcumuladaAux [] _ = []
sumaAcumuladaAux (x:xs) valAnterior = (x + valAnterior) : sumaAcumuladaAux xs (valAnterior + x)



--EJERCICIO 5.5
descomponerEnPrimos :: [Int] -> [[Int]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) = losPrimos x : descomponerEnPrimos xs

losPrimos :: Int -> [Int]
losPrimos n = factorizar n 2

factorizar :: Int -> Int -> [Int]
factorizar n d
  | n <= 1 = []
  | mod n d == 0 = d : factorizar (div n d) d
  | otherwise = factorizar n (d + 1)




--EJERCICIO 6.1
type Conjunto = [Integer]

agregarATodos :: Integer -> [Conjunto] -> [Conjunto]
agregarATodos _ [] = []
agregarATodos n (c:cls) = agregarAnAlConjunto n c : agregarATodos n cls

agregarAnAlConjunto :: Integer -> Conjunto -> Conjunto
agregarAnAlConjunto n subConjunto
  | pertenece n subConjunto = subConjunto
  | otherwise = n : subConjunto



--EJERCICIO 6.2

partes :: Integer -> [Conjunto]
partes n = reverso (partesAux [1..n])

partesAux :: [Integer] -> [Conjunto]
partesAux [] = [[]]
partesAux (x:xs) = partesConX ++ partesSinX
  where
    partesConX = agregarElemento x (partesAux xs)
    partesSinX = partesAux xs

agregarElemento :: Integer -> [Conjunto] -> [Conjunto]
agregarElemento _ [] = []
agregarElemento x (c:cs) = (x:c) : agregarElemento x cs


--EJERCICIO 6.3
type SetInteger = [Integer]
type parOrd = (Integer, Integer)

productoCartesiano :: SetInteger -> SetInteger -> [parOrd]
productoCartesiano [] _ = []
productoCartesiano (x:xs) ys = productoConElemento x ys ++ productoCartesiano xs ys

productoConElemento :: Integer -> SetInteger -> [parOrd]
productoConElemento _ [] = []
productoConElemento x (y:ys) = (x, y) : productoConElemento x ys


 