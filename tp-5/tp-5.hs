-- Cálculo de costos
-- Especificar el costo operacional de las siguientes funciones:

-- Costo O(1)
head' :: [a] -> a
head' (x:xs) = x

-- Costo O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

-- Costo O(n)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Costo O(n)
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Costo O(n^2)
factoriales :: [Int] -> [Int]
factoriales []     = []
factoriales (x:xs) = factorial x : factoriales xs

-- Costo O(n)
pertenece :: Eq a => a -> [a] -> Bool
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs

-- Costo O(n^2)
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if pertenece x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs

-- Costo O(n)
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

-- Costo O(n)
concatenar :: [String] -> String
concatenar []     = []
concatenar (x:xs) = x ++ concatenar xs

-- Costo O(n)
takeN :: Int -> [a] -> [a]
takeN 0 xs     = []
takeN n []     = []
takeN n (x:xs) = x : takeN (n-1) xs

-- 
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

-- Me faltan los costos
-- import SetV1
-- import SetV2

-- Como usuario del tipo abstracto Set implementar las siguientes funciones:
-- Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen al conjunto.
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     s = []
losQuePertenecen (e:es) s = if belongs e s
                                then e : losQuePertenecen es s
                                else losQuePertenecen es s

-- Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
-- sinRepetidos :: Eq a => [a] -> [a]
-- sinRepetidos []     = 
-- sinRepetidos (e:es) = ... e ... sinRepetidos es

-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT          = emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))

-- import QueueV1
-- import QueueV2
-- import QueueV3

-- Como usuario del tipo abstracto Queue implementar las siguientes funciones:
-- Cuenta la cantidad de elementos de la cola.
lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q
                then 0
                else 1 + (lengthQ (dequeue q))

-- Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
-- Nota: chequear que los elementos queden en el orden correcto.
queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q
                    then []
                    else firstQ q : (queueToList (dequeue q))

-- Inserta todos los elementos de la segunda cola en la primera.
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q2
                then ...
                else enqueue (firstQ q2) (unionQ q1 (dequeue q2))

-- import Stack

-- Como usuario del tipo abstracto Stack implementar las siguientes funciones:
-- Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar []     = emptySt
apilar (e:es) = push e (apilar es)

-- Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar st = if isEmptySt 
                then []
                else top : (desapilar (pop st))

-- Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha posición (se desapilan elementos hasta dicha posición y 
-- se inserta en ese lugar).
-- insertarEnPos :: Int -> a -> Stack a -> Stack a -- PRECOND.: la posición debe ser válida en la lista.
-- insertarEnPos 0 e st = ...
-- insertarEnPos n e st = ... n ... e ... insertarEnPos (n-1) e st