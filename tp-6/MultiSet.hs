import MapV1
-- import MapV2
-- import MapV3

module MultiSet (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList) where

-- Un MultiSet (multiconjunto) es un tipo abstracto de datos similar a un Set (conjunto). A diferencia del último, cada elemento posee una 
-- cantidad de apariciones, que llamaremos ocurrencias del elemento en el multiset.
-- Implementar el tipo abstracto MultiSet utilizando como representación un Map. Indicar los ordenes de complejidad en peor caso de cada 
-- función de la interfaz, justificando las respuestas.

data MultiSet a = MS (Map a Int) -- Int puede ser < 0 ??

emptyMS :: MultiSet a
-- Propósito: denota un multiconjunto vacío.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
-- (opcional) Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
-- (opcional) Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
multiSetToList :: MultiSet a -> [(a, Int)]
-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.

emptyMs                         = MS emptyM                            -- O(1)
addMS            x     (MS map) = MS (sumarOcurrencia x map)           -- O()
ocurrencesMS     x     (MS map) = lookupM x map                        -- O()
unionMS         ms1     ms2     = error "HACER"                        -- O()
intersectionMS  ms1     ms2     = error "HACER"                        -- O()
multiSetToList (MS map)         = mapToList map                        -- O()

-- Con MapV1 --> assocM = O(n), lookupM = O(n)    => sumarOcurrencia tiene costo O()
-- Con MapV2 --> assocM = O(1), lookupM = O(n)    => sumarOcurrencia tiene costo O()
-- Con MapV3 --> assocM = O(1), lookupM = O(n^2)  => sumarOcurrencia tiene costo O()
sumarOcurrencia :: Ord a => a -> Map a Int -> Map a Int
sumarOcurrencia x map = assocM x ((lookupM x map)+1) map

 -- O(1), el costo de abrir la estructura es constante
getMap :: MultiSet -> Map a Int
getMap (MS map) = map

-- Con MapV1 --> domM = O() => mapToList tiene costo O()
-- Con MapV2 --> domM = O() => mapToList tiene costo O()
-- Con MapV3 --> domM = O() => mapToList tiene costo O()
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = asociarClaves (domM map) map

-- Con MapV1 --> lookupM = O(n)   => asociarClaves tiene costo O()
-- Con MapV2 --> lookupM = O(n)   => asociarClaves tiene costo O()
-- Con MapV3 --> lookupM = O(n^2) => asociarClaves tiene costo O()
asociarClaves :: Eq k => [k] -> Map k v -> [(k,v)]
asociarClaves []     map = []
asociarClaves (k:ks) map = (k, lookupM k map) : asociarClaves ks map