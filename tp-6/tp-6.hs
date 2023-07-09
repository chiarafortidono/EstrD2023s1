import PriorityQueue
import MapV1
-- import MapV2
-- import MapV3
import MultiSet

-- Ejercicio 2
-- Implementar la siguiente función, que dada una lista la ordena de menor a mayor utilizando una Priority Queue 
-- como estructura auxiliar. Cuál es su costo? 
-- OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority Queues basada en una estructura concreta 
-- llamada Heap, que será trabajada en la siguiente práctica.
-- O(n^2), debe recorrer dos veces n elementos (una vez en la lista y la otra en la PQ).
heapSort :: Ord a => [a] -> [a]
heapSort xs = pqAListaOrdenada (listaAPQ xs)

-- O(n), siendo n la cantidad de elementos en la PQ.
pqAListaOrdenada :: Ord a => PriorityQueue a -> [a]
pqAListaOrdenada pq = if isEmptyPQ pq
                        then []
                        else findMinPQ pq : (pqAListaOrdenada (deleteMinPQ pq))

-- O(n), siendo n la longitud de la lista.
listaAPQ :: Ord a => [a] -> PriorityQueue a
listaAPQ []     = emptyPQ
listaAPQ (x:xs) = insertPQ x (listaAPQ xs)

-- Ejercicio 3
-- Propósito: obtiene los valores asociados a cada clave del map.
-- valores tiene costo O(n) y el costo de domM depende de la implementación del TAD Map
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = valores (domM map)

-- O(n), siendo n la longitud de la lista.
valores :: Eq k => [k] -> [Maybe v]
valores []     = []
valores (k:ks) = lookupM k : valores ks

-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas []     map = True
todasAsociadas (k:ks) map = elem k (domM map) && todasAsociadas ks map

-- Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

-- Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = asociarClaves (domM map) map

asociarClaves :: Eq k => [k] -> Map k v -> [(k,v)]
asociarClaves []     map = []
asociarClaves (k:ks) map = (k, lookupM k map) : asociarClaves ks map

-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
-- agruparEq :: Eq k => [(k, v)] -> Map k [v]

-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar []     map = map
incrementar (k:ks) map = assocM k ((lookupM k map)+1) (incrementar ks map)

-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo, 
-- es reemplazada por la del primero.
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = asociarClavesEnMap (domM map2) map2 map1

asociarClavesEnMap :: Eq k => [k] -> Map k v -> Map k v -> Map k v
asociarClavesEnMap []     map2 map1 = map1
asociarClavesEnMap (k:ks) map2 map1 = assocM k (lookupM k map2) (asociarClavesEnMap ks map2 map1)

-- Ejercicio 5
-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista.
-- Pensar algo con el indexar de la teórica 4, que indexa una lista de a en una lista de tuplas.
-- indexar :: [a] -> Map Int a

-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen en el string, y los valores la cantidad de 
-- veces que aparecen en el mismo.
-- ocurrencias :: String -> Map Char Int

-- Ejercicio 6
-- Propósito: dado un string, devuelve un MultiSet de caracteres que aparecen en el string.
ocurrencias' :: String -> MultiSet Char
ocurrencias' []     = emptyMS
ocurrencias' (c:cs) = addMS c (ocurrencias' cs)