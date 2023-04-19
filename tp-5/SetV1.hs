module SetV1 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) where

-- Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda la cantidad de elementos en la estructura.
-- Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones de esta implementación, pero para mantener una
-- interfaz común entre distintas posibles implementaciones estamos obligados a escribir así los tipos.

data Set a = S [a] (Int)
{- INV. REP.: en S es n se cumple que:
    * n es la longitud de es.
    * los elementos en es no se repiten.
-}

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

emptyS             = S [] 0                                                     -- Costo O(1)
addS    e (S es n) = S (agregar e es) (n+1)                                     -- Costo O(n)
belongs e (S es n) = elem e es                                                  -- Costo O(n)
sizeS     (S es n) = n                                                          -- Costo O(1)
removeS e (S es n) = S (quitar e es)  (n-1)                                     -- Costo O(n)
unionS s1 s2       = S ((setToList s1)++(setToList s2)) ((sizeS s1)+(sizeS s2)) -- Costo O(1)
setToList (S es n) = es                                                         -- Costo O(1)

-- Costo O(n)
agregar :: Eq a => a -> [a] -> [a]
agregar e []     = [e]
agregar e (x:xs) = if e == x
                    then e : xs
                    else x : agregar e xs

-- Costo O(n)
quitar :: Eq a => a -> [a] -> [a]
quitar e []     = []
quitar e (x:xs) = if e == x
                    then xs
                    else quitar e xs