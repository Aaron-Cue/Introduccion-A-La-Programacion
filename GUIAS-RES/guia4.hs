--EJERCICIO 1
fibonacci :: Int -> Int
fibonacci i | i == 0 = 0 
            | i == 1 = 1
            | i > 0 = (fibonacci (i - 1)) + (fibonacci (i - 2))


--EJERCICIO 2
parteEntera :: Float -> Int
parteEntera x | x >= 0    = floor x 
              | otherwise = ceiling x


--EJERCICIO 3
esDivisible :: Integer -> Integer -> Bool
esDivisible n1 n2 | n1 == n2 = True
                  | n1 == 0 = True
                  | n2 == 0 = False
                  | n1 < n2 = False
                  | otherwise = esDivisible (n1 - n2) n2 

                  
--EJERCICIO 4
sumaImpares :: Int -> Int
sumaImpares n | n == 0 = 0 
              | n == 1 = 1
              | otherwise = (2*n - 1) + sumaImpares (n - 1)

--sin recursividad
sumaImpares' :: Int -> Int
sumaImpares' n = sum [2*i - 1 | i <- [1..n]]


--EJERCICIO 5
medioFact :: Int -> Int
medioFact n | n == 0 = 1
            | n == 1 = 1
            | otherwise = n * medioFact (n - 2)


--EJERCICIO 6
sumaDigitos :: Int -> Int
sumaDigitos n | n < 10 = n
              | otherwise = (sumaDigitos (div n 10)) + (mod n 10)



--EJERCICIO 7
todosDigitosIguales :: Int -> Bool
todosDigitosIguales n = if n < 10 then True
                        else if mod n 10 == mod (div n 10) 10
                             then todosDigitosIguales (div n 10)
                             else False     


--EJERCICIO 8 
iesimoDigito :: Int -> Int -> Int
iesimoDigito n i = if i == cantDigitos n then mod n 10 else iesimoDigito (div n 10) i               


cantDigitos :: Int -> Int 
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (div n 10)



--EJERCICIO 9
esCapicua :: Int -> Bool
esCapicua n = if (enteroALista n) == reverse (enteroALista n) then True else False


--entero a lista
enteroALista :: Int -> [Int]
enteroALista n | n < 10 = [n]
               | otherwise = enteroALista (div n 10) ++ [(mod n 10)] 



--EJERCICIO 10
  -- a)
f1 :: Int -> Int 
f1 n | n == 0 = 1
     | otherwise = (2^n) + f1 (n-1)


  -- b)
f2 :: Int -> Float -> Float
f2 n x | n == 1 = x
       | otherwise = x^n + f2 (n-1) x


  -- c) 
f3 :: Int -> Float -> Float
f3 n x | n == 1 = x + x^2
       | otherwise = x^(2*n) + x^((2*n)-1) + f3 (n-1) x


  -- d)
f4 :: Int -> Float -> Float
f4 n x = f4Aux n (2*n) x

f4Aux :: Int -> Int -> Float -> Float  -- una sumatoria que toma como argumentos los rangos de i y el x a elevar
f4Aux n m x                            -- n (desde) m (hasta)
  | n > m     = 0                            
  | otherwise = (x^n) + f4Aux (n+1) m x



-- EJERCICIO 11
-- a) 
eAprox :: Int -> Float
eAprox n | n == 0 = 1
         | otherwise = 1/(fact' (fromIntegral n)) + eAprox (n-1)


fact' :: Float -> Float
fact' n | n == 0 = 1
        | otherwise = n * fact' (n-1)


-- b) 
e :: Float 
e = eAprox 10



--EJERCICIO 12
sucesion_an :: Int -> Float
sucesion_an n | n == 1 = 2
              | otherwise = 2 + 1/(sucesion_an (n-1))


raizDe2Aprox :: Int -> Float
raizDe2Aprox n = (sucesion_an n) - 1



--EJERCICIO 13

dobleSumatoria :: Int -> Int -> Int
dobleSumatoria n m = sumatoriaAux n m 1

sumatoriaAux :: Int -> Int -> Int -> Int
sumatoriaAux n m i
  | i > n = 0
  | otherwise = sumatoriaAux n m (i+1) + sumatoriaAuxAux m i 1

sumatoriaAuxAux :: Int -> Int -> Int -> Int
sumatoriaAuxAux m i j
  | j > m = 0
  | otherwise = i^j + sumatoriaAuxAux m i (j+1)




--EJERCICIO 14
{- sumaPotencias :: Int -> Int -> Int -> Int
sumaPotencias q n m | q == 0 = 1
                    | otherwise = -} 




 --EJERCICIO 15
sumaRacionales :: Int -> Int -> Int
sumaRacionales n m = sumAux n m 1

sumAux :: Int -> Int -> Int -> Int
sumAux n m p
  | p > n = 0
  | otherwise = sumAux n m (p+1) + sumAuxAux m p 1

sumAuxAux :: Int -> Int -> Int -> Int
sumAuxAux m p q
  | q > m = 0
  | otherwise = div p q + sumAuxAux m p (q+1)



--EJERCICIO 16
  -- a)
menorDivisor :: Int -> Int
menorDivisor n | esPrimo n = n
               | esPar n = 2
               | otherwise = head [x | x <- [3,5..n], mod n x == 0]
               where esPar n = if mod n 2 == 0 then True else False

--con recursion
menorDivisor' :: Int -> Int
menorDivisor' n | esPrimo n = n
                | esPar n = 2
                | otherwise = buscarDivisor n 3
                where esPar n = if mod n 2 == 0 then True else False
                      buscarDivisor n divisor | mod n divisor == 0 = divisor 
                                              | divisor^2 > n = n
                                              | otherwise = buscarDivisor n (divisor + 2)



  -- b)
esPrimo :: Int -> Bool
esPrimo n
  | n <= 1 = False 
  | otherwise = esPrimoAux n (n-1)

esPrimoAux :: Int -> Int -> Bool
esPrimoAux n k | k == 1 = True  
               | mod n k == 0 = False  primo
               | otherwise = esPrimoAux n (k-1)



  -- c)
sonCoprimos :: Int -> Int -> Bool  -- si tienen algun divisor en comun es false
sonCoprimos n m | n == m = False
                | (esPar n) && (esPar m) = False
                | gcd n m > 1 = False
                | (esPrimo n) && (esPrimo m) = True
                | otherwise = True
                where esPar n = if mod n 2 == 0 then True else False


  -- d)
nEsimoPrimo :: Int -> Int
nEsimoPrimo n = [x | x <- [1,2..(n+200)], (esPrimo x)] !! (n-1)           --funciona pero claramente hay que corregir el (n+200)




--EJERCICIO 17
esFibonacci :: Int -> Bool
esFibonacci n = esFibonacciAux n 0 1

esFibonacciAux :: Int -> Int -> Int -> Bool
esFibonacciAux n fibAnterior fibActual
  | fibActual == n = True
  | fibActual > n = False
  | otherwise = esFibonacciAux n fibActual (fibAnterior + fibActual)




--EJERCICIO 18
mayorDigitoPar :: Int -> Int  
mayorDigitoPar n
  | n < 10 && esPar n = n            
  | esPar resto = max resto (mayorDigitoPar (div n 10))
  | otherwise = mayorDigitoPar (div n 10)
  where
    resto = mod n 10
    esPar n = mod n 2 == 0



--EJERCICIO 19
--esSumaInicialDePrimos :: Int -> Bool
--esSumaInicialDePrimos n = 



--EJERCICIO 20
tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 = go n1 n1 0
  where
    go m i maxSum
      | i > n2 = m
      | sumDivisores i > maxSum = go i (i + 1) (sumDivisores i)
      | otherwise = go m (i + 1) maxSum

sumDivisores :: Int -> Int
sumDivisores n = sum [x | x <- [1..n `div` 2], n `mod` x == 0] + n



--EJERCICIO 21
pitagoras :: Integer -> Integer -> Integer -> Integer
pitagoras m n r = contadorPitagoricos m n r

esPitagorico :: Integer -> Integer -> Integer -> Bool
esPitagorico p q r = (p^2 + q^2) <= (r^2)

contadorPitagoricos :: Integer -> Integer -> Integer -> Integer
contadorPitagoricos m n r = sum [1 | p <- [0..m], q <- [0..n], esPitagorico p q r]








