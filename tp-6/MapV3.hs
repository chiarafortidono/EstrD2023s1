module MapV2 (Map, emptyM, assocM, lookupM, deleteM, keys) where

-- Cada clave k, se asocia a un valor v.
-- Implementación con dos listas: una de claves y otra de valores, que tienen la misma longitud.

data Map k v = M [k] [v]
{- INV. REP.: en M ks vs, se cumple que 
    * ks y vs tienen la misma longitud.
    * la clave en la posición i de la lista ks está asociado al valor en la posición i de la lista vs.
-}

emptyM  :: Map k v
assocM  :: Ord k => k -> v -> Map k v -> Map k v -- si la clave está asociada a otro valor, lo sobrescribe.
lookupM :: Ord k => k -> Map k v -> Maybe v
deleteM :: Ord k => k -> Map k v -> Map k v
domM    :: Ord k => Map k v -> [k]

emptyM                = M [] []          -- O(1), crear la estructura y la lista es O(1).
assocM  k v (M ks vs) = M (k:ks) (v:vs)  -- O(1), cons es O(1). 
                                         -- Puedo hacer cons directamente por el invariante (length ks == length vs)
lookupM k   (M ks vs) = if null ks       -- O(n^2), siendo n la posición del k que busco y del v asociado.
                            then Nothing
                            else valorEnPosicion (indiceDeEn k ks) vs
deleteM k   (M ks vs) = M (borrar k ks) (borrarPosicion (indiceDeEn k ks) vs) 
                        -- O(n^2), siendo n la posición donde se encuentra k en ks y v en vs.
domM        (M ks vs) = ks               -- O(1), la lista es parte de la estructura. 

-- O(n) siendo n la posición de la cual estoy buscando el valor, que es tambien la posición de k en ks.
valorEnPosicion :: Ord k => Int -> [v] -> Maybe v
valorEnPosicion 0 vs = head vs  -- no tengo que chequear si es null vs por el invariante, ya lo chequeé
                                -- en el if null ks. si ks es null, vs también.
valorEnPosicion n vs = valorEnPosicion (n-1) (tail vs)

-- O(n) siendo n la posición en la que se encuentra k, que es también la posición de v en vs.
indiceDeEn :: Ord k => k -> [k] -> Int
indiceDeEn k (k':ks) = if k == k'
                        then 0
                        else 1 + indiceDeEn k ks

-- O(n), siendo n la posición en la que se encuentra k.
borrar :: Ord k => k -> [k] -> [k]
borrar k []      = []
borrar k (k':ks) = if k == k'
                    then borrar k ks 
                    else k' : borrar k kvs

-- O(n), siendo n la posición que quiero borrar.
borrarPosicion :: Ord k => Int -> [v] -> [v]
borrarPosicion 0 [] = []
borrarPosicion 0 vs = tail vs
borrarPosicion n vs = borrarPosicion (n-1) vs