data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
{- INV. REP.: en (MkN mss ht si)
    * un tripulante no puede estar en dos o más conjuntos asociados a 
    múltiples claves del map.
    * la primera componente del par si está asociada en el map al 
    conjunto con mayor cardinalidad y dicha cardinalidad es igual a la
    segunda componente del par si.
    * cada tripulante que está en un conjunto como valor del map debe
    estar presente en la heap y viceversa.
-}
{- OBS.: 
    * Cada tripulante puede estar en un sector como máximo.
    * Se guarda al sector con más tripulantes de la nave y cuántos 
    tripulantes tiene ese sector.
    * Los tripulantes se ordenan por rango de mayor a menor en la Heap.
    * No se confunda, findMin devuelve al tripulante con mayor rango.
    * No puede existir una nave que no tenga al menos un sector.
-}

-- Nunca se aclaran en el inv. rep. cosas que están dadas por la 
-- estructura. Ejemplo: la heap está ordenada de mayor a menor.

-- Propósito: Crea una nave con todos esos sectores sin tripulantes.
-- Precondición: la lista de sectores no está vacía
-- Costo: O(S log S) siendo S la cantidad de sectores de la lista.
naveVacia :: [Sector] -> Nave
naveVacia ss = MkN (asKeys ss) emptyH (head ss, 0)

asKeys :: Eq k => [k] -> Map k (Set v)
asKeys []     = emptyM
asKeys (x:xs) = assocM x emptyS (asKeys xs)

-- Propósito: Obtiene los tripulantes de un sector.
-- Costo: O(log S) siendo S la cantidad de sectores.
-- Debería agregarse una precondición porque no es lo mismo devolver
-- el error que devolver un emptyS, son resultados distintos.
tripulantesDe :: Sector -> Nave -> Set Tripulante
tripulantesDe s (MkN mss ht si) = case (lookupM s mss) of 
                                    Nothing  -> error "No existe el sector"
                                    Just st  -> st

-- Propósito: Denota los sectores de la nave
-- Costo: O(S) siendo S la cantidad de sectores.
sectores :: Nave -> [Sector]
sectores (MkN mss ht si) = keys mss

-- Propósito: Denota el tripulante con mayor rango.
-- Precondición: la nave no está vacía.
-- Costo: O(1).
conMayorRango :: Nave -> Tripulante
conMayorRango (MkN mss ht si) = findMin ht

-- Propósito: Denota el sector de la nave con más tripulantes.
-- Costo: O(1).
conMasTripulantes :: Nave -> Sector
conMasTripulantes (MkN mss ht si) = fst si

-- Propósito: Denota el conjunto de tripulantes con dicho rango.
-- Costo: O(P log P) siendo P la cantidad de tripulantes.
conRango :: Rango -> Nave -> Set Tripulante
conRango r (MkN mss ht si) = filtrarConRango r ht

filtrarConRango :: Rango -> Heap Tripulante -> Set Tripulante
filtrarConRango r ht = if isEmptyH ht
                        then emptyS
                        else agregarSiMismoRango r (findMinH ht) (filtrarConRango r (deleteMin ht))
                            
agregarSiMismoRango :: Rango -> Tripulante -> Set Tripulante -> Set Tripulante
agregarSiMismoRango r t ts = if rango t == r
                                then addS t ts
                                else ts

-- Propósito: Devuelve el sector en el que se encuentra un tripulante.
-- Precondición: el tripulante pertenece a la nave.
-- Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
sectorDe :: Tripulante -> Nave -> Sector
sectorDe t (Mkn mss ht si) = sectorDeRec t (keys mss) mss

-- Precondición: el valor está en alguno de los conjuntos asociado a
-- alguna de las claves. La lista corresponde a las claves del map.
sectorDeRec :: Ord k => v -> [k] -> Map k (Set v) -> k
sectorDeRec t [s]    mss = s -- Garantizado por la precondición.
sectorDeRec t (s:ss) mss = case (lookupM s mss) of
                            Nothing -> error "No se encontró la clave"
                            Just st -> if (belongs t st)
                                        then s
                                        else sectorDeRec t ss mss

-- Propósito: Agrega un tripulante a ese sector de la nave.
-- Precondición: El sector está en la nave y el tripulante no.
-- Costo: No hay datos (justifique su elección).
agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
agregarTripulante t s (MkN mss ht si) = case (lookupM s mss) of
                                            Nothing -> MkN (assocM s (addS t emptyS) mss) (insertH t ht) (maxT (s, sizeS ts + 1) si) 
                                            Just st -> MkN (assocM s (addS t st) mss)     (insertH t ht) (maxT (s, 1) si) 

maxT :: Ord b => (a, b) -> (a, b) -> (a, b)
max (x1, z1) (x2, z2) = if z1 > z2
                            then (x1, z1)
                            else (x2, z2)