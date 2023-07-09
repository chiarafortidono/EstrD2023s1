module MapV1 (Map, emptyM, assocM, lookupM, deleteM, keys) where

-- Cada clave k, se asocia a un valor v.
-- Implementación con lista de pares (k, v), sin claves repetidas

data Map k v = M [(k,v)]
 {- INV. REP.: en M kvs, no hay claves repetidas en kvs.
 -}

emptyM  :: Map k v
assocM  :: Ord k => k -> v -> Map k v -> Map k v -- si la clave está asociada a otro valor, lo sobrescribe.
lookupM :: Ord k => k -> Map k v -> Maybe v
deleteM :: Ord k => k -> Map k v -> Map k v
domM    :: Ord k => Map k v -> [k]

emptyM              = M []                  -- O(1)
assocM  k v (M kvs) = M (asociar k v kvs)   -- O(n)
lookupM k   (M kvs) = buscar k kvs          -- O(n)
deleteM k   (M kvs) = M (borrar k kvs)      -- O(n)
domM        (M kvs) = claves kvs            -- O(n)

-- O(n)
buscar :: Eq k => k -> [(k, v)] -> Maybe v
buscar k []            = Nothing
buscar k ((k',v'):kvs) = if k == k' 
                            then Just v'
                            else buscar k kvs

-- O(n)
claves :: [(k,v)] -> [k]
claves []          = []
claves ((k,v):kvs) = k : claves kvs

-- O(n)
asociar :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v []            = 
asociar k v ((k',v'):kvs) = if k == k'
                                then (k',v)  : kvs
                                else (k',v') : asociar k v kvs

-- O(n)
borrar :: Eq k => k -> [(k,v)] -> [(k,v)]
borrar k []            = []
borrar k ((k',v'):kvs) = if k == k'
                            then kvs
                            else (k',v') : borrar k kvs