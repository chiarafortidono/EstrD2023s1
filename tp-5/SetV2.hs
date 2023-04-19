module SetV2 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

-- Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En otras palabras, al agregar no va a chequear que 
-- si el elemento ya se encuentra en la lista, pero sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
-- por ejemplo). Contrastar la eficiencia obtenida en esta implementación con la anterior.

data Set a = S [a]

-- 2. Set (conjunto)
-- Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:

-- Crea un conjunto vacío.
emptyS :: Set a 
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a 
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS :: Eq a => Set a -> Int
-- Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList :: Eq a => Set a -> [a]

emptyS           = S []                                 -- Costo O(1)
addS    e (S es) = S (e:es)                             -- Costo O(1)
belongs e (S es) = elem e es                            -- Costo O(n)
sizeS     (S es) = length (sinDuplicados es)            -- Costo O(n^2)
removeS e (S es) = if elem e es                         -- Costo O(n)
                        then Set es
                        else Set (quitar e es)
unionS s1  s2    = S ((setToList s1)++(setToList s2))   -- Costo O(n)
setToList (S es) = sinDuplicados es                     -- Costo O(n)

-- Costo O(n)
sinDuplicados :: Eq a => [a] -> [a]
sinDuplicados []     = []
sinDuplicados (x:[]) = (x:[])
sinDuplicados (x:xs) = if pertenece x xs 
                        then sinDuplicados xs
                        else x : sinDuplicados xs

-- Costo O(n)
quitar :: Eq a => a -> [a] -> [a]
quitar e []     = []
quitar e (x:xs) = if e == x
                    then xs
                    else quitar e xs