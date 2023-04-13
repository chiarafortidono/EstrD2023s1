-- Cálculo de costos
-- Especificar el costo operacional de las siguientes funciones:

head' :: [a] -> a
head' (x:xs) = x

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

factoriales :: [Int] -> [Int]
factoriales []     = []
factoriales (x:xs) = factorial x : factoriales xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if pertenece x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

concatenar :: [String] -> String
concatenar []     = []
concatenar (x:xs) = x ++ concatenar xs

takeN :: Int -> [a] -> [a]
takeN 0 xs     = []
takeN n []     = []
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a]
dropN 0 xs     = xs
dropN n []     = []
dropN n (x:xs) = dropN (n-1) xs

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

minimo :: Ord a => [a] -> a
minimo [x]    = x
minimo (x:xs) = min x (minimo xs)

sacar :: Eq a => a -> [a] -> [a]
sacar n []     = []
sacar n (x:xs) = if n == x
                    then xs
                    else x : sacar n xs

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs = let m = minimo xs
             in m : ordenar (sacar m xs)

-- import SetV1
-- import SetV2

-- Como usuario del tipo abstracto Set implementar las siguientes funciones:
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
-- losQuePertenecen :: Eq a => [a] -> Set a -> [a]

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
-- sinRepetidos :: Eq a => [a] -> [a]

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
-- unirTodos :: Eq a => Tree (Set a) -> Set a

-- import QueueV1
-- import QueueV2
-- import QueueV3

-- Como usuario del tipo abstracto Queue implementar las siguientes funciones:
-- Cuenta la cantidad de elementos de la cola.
-- lengthQ :: Queue a -> Int

-- Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto.
-- queueToList :: Queue a -> [a]

-- Inserta todos los elementos de la segunda cola en la primera.
-- unionQ :: Queue a -> Queue a -> Queue a

import Stack

-- Como usuario del tipo abstracto Stack implementar las siguientes funciones:
-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
-- apilar :: [a] -> Stack a

-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
-- desapilar :: Stack a -> [a]

-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha posición (se desapilan elementos hasta dicha posición y 
-- se inserta en ese lugar).
-- insertarEnPos :: Int -> a -> Stack a -> Stack a