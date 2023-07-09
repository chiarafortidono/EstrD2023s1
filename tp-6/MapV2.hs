module MapV2 (Map, emptyM, assocM, lookupM, deleteM, keys) where

-- Cada clave k, se asocia a un valor v.
-- Implementación con lista de pares (k, v), con claves repetidas

data Map k v = M [(k,v)]

emptyM  :: Map k v
assocM  :: Ord k => k -> v -> Map k v -> Map k v -- si la clave está asociada a otro valor, lo sobrescribe.
lookupM :: Ord k => k -> Map k v -> Maybe v
deleteM :: Ord k => k -> Map k v -> Map k v
domM    :: Ord k => Map k v -> [k]

emptyM              = M []                       -- O(1), crear la estructura y la lista es O(1).
assocM  k v (M kvs) = M ((k,v):kvs)              -- O(1), cons es O(1).
lookupM k   (M kvs) = buscar k kvs               -- O(n), buscar es O(n).
deleteM k   (M kvs) = M (borrar k kvs)           -- O(n), borrar es O(n) y crear la estructura también.
domM        (M kvs) = sinDuplicados (claves kvs) -- En peor caso, no hay claves duplicadas y recorre la misma lista de longitud n dos veces, 
                                                 -- por lo tanto, el costo es O(n^2)

-- O(n), por cada elemento de la lista en peor caso hace una operación constante (la comparación por igual).
-- Siendo n la longitud de la lista.
buscar :: Ord k => k -> [(k,v)] -> Maybe v
buscar k []            = Nothing
buscar k ((k',v'):kvs) = if k == k'
                            then Just v'
                            else buscar k kvs

-- O(n), por cada elemento de la lista en peor caso hace una operación constante (la comparación por igual).
-- Siendo n la longitud de la lista.
borrar :: Ord k => k -> [(k,v)] -> [(k,v)]
borrar k []            = []
borrar k ((k',v'):kvs) = if k == k'
                            then borrar k kvs 
                            else (k',v') : borrar k kvs

-- O(n), por cada elemento de la lista en peor caso hace una operación constante (la comparación por igual).
-- Siendo n la longitud de la lista.
claves :: [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) = k : claves kvs

-- O(n), por cada elemento de la lista en peor caso hace una operación constante (la comparación por igual).
-- Siendo n la longitud de la lista.
sinDuplicados :: Eq a => [a] -> [a]
sinDuplicados []     = []
sinDuplicados (x:[]) = [x]
sinDuplicados (x:xs) = if elem x xs
                        then sinDuplicados xs
                        else x : sinDuplicados xs