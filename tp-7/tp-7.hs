-- Ejercicio 1
-- Indicar el costo de heapsort :: Ord a => [a] -> [a] (de la práctica anterior) suponiendo que el usuario utiliza una priority queue con costos 
-- logarítmicos de inserción y borrado (o sea, usa una Heap como tipo de representación).
-- El costo de pqAListaOrdenada sería O(n log n), siendo n los elementos de la PQ.
-- El costo de listaAPQ sería O(n log n), siendo n los elementos de la lista (los futuros elementos de la PQ).
-- Por lo tanto, el costo de heapSort sería O(2(n log n)), la constante se desestima y el costo final sería O(n log n).

-- Ejercicio 2
-- Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los invariantes de BST y sin elementos repetidos 
-- (despreocuparse por el hecho de que el árbol puede desbalancearse al insertar o borrar elementos). En todos los costos, N es la cantidad de 
-- elementos del árbol. Justificar por qué la implementación satisface los costos dados.

-- Propósito: dado un BST dice si el elemento pertenece o no al árbol. Costo: O(log N)
-- El costo es O(log N), siendo N la cantidad de elementos del árbol, porque sólo se recorre una rama del arbol.
belongsBST :: Ord a => a -> Tree a -> Bool -- PRECOND.: el árbol es BST.
belongsBST e EmptyT          = False
belongsBST e (NodeT x ti td) = if e > x
                                then belongsBST e td || e == x
                                else belongsBST e ti 

-- Propósito: dado un BST inserta un elemento en el árbol. Costo: O(log N)
-- El costo es O(log N), siendo N la cantidad de elementos del árbol, porque sólo se recorre una rama del arbol.
insertBST :: Ord a => a -> Tree a -> Tree a -- PRECOND.: el árbol es BST.
insertBST e EmptyT          = NodeT e EmptyT EmptyT
insertBST e (NodeT x ti td) = if e == x 
                                then NodeT e ti td
                                else if e < x
                                        then NodeT x (insertBST e ti) td
                                        else NodeT x ti (insertBST e td)

-- Propósito: dado un BST borra un elemento en el árbol. Costo: O(log N)
-- El costo es O(log N), siendo N la cantidad de elementos del árbol, porque sólo se recorre una rama del arbol.
deleteBST :: Ord a => a -> Tree a -> Tree a -- PRECOND.: el árbol es BST.
deleteBST e EmptyT          = EmptyT
deleteBST e (NodeT x ti td) = if e == x
                                then rearmarBST ti td
                                else if e < x
                                        then NodeT x (deleteBST e ti) td
                                        else NodeT x ti (deleteBST e td)

-- O(log N), porque splitMaxBST tiene costo O(log N)
rearmarBST :: Ord a => Tree a -> Tree a -> Tree a -- PRECOND.: los árboles son BSTs.
rearmarBST EmptyT td = 
rearmarBST ti     td = let (m, ti') = splitMaxBST ti
                        in NodeT m ti' td

-- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo. Costo: O(log N)
-- 
splitMinBST :: Ord a => Tree a -> (a, Tree a) -- PRECOND.: el árbol es BST y no está vacío.
splitMinBST (NodeT x ti EmptyT) = (x, ti)
splitMinBST (NodeT x ti td)     = let (m, ti') = splitMinBST ti
                                   in (m, (NodeT x ti' td))

-- Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo. Costo: O(log N)
-- 
splitMaxBST :: Ord a => Tree a -> (a, Tree a) -- PRECOND.: el árbol es BST y no está vacío.
splitMaxBST (NodeT x ti EmptyT) = (x, td)
splitMaxBST (NodeT x ti td)     = let (m, td') = splitMaxBST td
                                   in (m, (NodeT x ti td'))

-- Propósito: indica si el árbol cumple con los invariantes de BST. Costo: O(N^2)
-- El costo es O(n^2) porque el árbol se recorre completo y se usan dos funciones con costo O(n).
esBST :: Tree a -> Bool
esBST EmptyT          = True
esBST (NodeT x ti td) = esMayorATodos x ti && esMenorATodos x td && esBST ti && esBST td

-- Costo O(n) siendo n la cantidad de elementos del árbol, porque recorre todos los elementos del árbol
esMayorATodos :: Ord a => a -> Tree a -> Bool
esMayorATodos e EmptyT          = True
esMayorATodos e (NodeT x ti td) = e > x && esMayorATodos e ti && esMayorATodos e td

-- Costo O(n) siendo n la cantidad de elementos del árbol, porque recorre todos los elementos del árbol
esMenorATodos :: Ord a => a -> Tree a -> Bool
esMenorATodos e EmptyT          = True
esMenorATodos e (NodeT x ti td) = e < x && esMenorATodos e ti && esMenorATodos e td

-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado. Costo: O(log N)
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA e EmptyT          = Nothing
elMaximoMenorA e (NodeT x ti td) = if e > x 
                                    then elegirMayorEntre e (elMaximoMenorA e td) 
                                    else elMaximoMenorA e ti

-- Costo: O(1)
elegirMayorEntre :: Ord a => a -> Maybe a -> Maybe a
elegirMayorEntre x Nothing  = Just x
elegirMayorEntre x (Just y) = if x > y then (Just x) else (Just y)

-- Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado. Costo: O(log N)
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA e EmptyT          = Nothing
elMinimoMayorA e (NodeT x ti td) = if e > x
                                    then elMinimoMayorA e ti
                                    else elegirMenorEntre e (elMinimoMayorA e td)

-- Costo: O(1)
elegirMenorEntre :: Ord a => a -> Maybe a -> Maybe a
elegirMenorEntre x Nothing  = Just x
elegirMenorEntre x (Just y) = if x < y then (Just x) else (Just y)

-- Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la diferencia de alturas entre el subarbol 
-- izquierdo y el derecho es menor o igual a 1. Costo: O(N^2)
balanceado :: Tree a -> Bool
balanceado EmptyT          = True
balanceado (NodeT x ti td) = abs((altura ti)-(altura td)) <= 1 && balanceado ti && balanceado td

altura :: Tree a -> Int
altura EmptyT          = 0
altura (NodeT x ti td) = 1 + max (altura ti) (altura td)

-- Ejercicio 3
-- Dada la siguiente interfaz y costos para el tipo abstracto Map:
-- emptyM  :: Map k v                                 -- Costo: O(1).
-- assocM  :: Ord k => k -> v -> Map k v -> Map k v   -- Costo: O(log K).
-- lookupM :: Ord k => k -> Map k v -> Maybe v        -- Costo: O(log K).
-- deleteM :: Ord k => k -> Map k v -> Map k v        -- Costo: O(log K).
-- keys    :: Map k v -> [k]                          -- Costo: O(K).
-- recalcular el costo de las funciones como usuario de Map de la práctica anterior, siendo K la cantidad de claves del Map. 
-- Justificar las respuestas.

-- valuesM queda con costo O(k log k), siendo k la cantidad de claves del map, porque por cada clave del map, se hace 
-- el lookup que tiene costo O(log k)

-- todasAsociadas queda con costo O(k^n), siendo k la cantidad de claves del map y n la longitud de la lista, porque
-- por cada elemento de la lista se hace una operación con costo O(k), que es domM

-- listToMap queda con costo O(n log k), siendo n la longitud de la lista de pares y k la cantidad de claves del map,
-- porque se recorre toda la lista haciendo una operación con costo O(log k), assocM, por cada par.

-- mapToList queda con costo O(k + k log k), siendo k la cantidad de claves del map, porque por cada clave del map se
-- hace una operación logarítmica (lookupM) y además el domM que se suma tiene costo lineal sobre las claves.

-- agruparEq queda con costo O()

-- incrementar queda con costo O(n log k), siendo n la longitud de la lista y k la cantidad de claves del map, porque
-- por cada clave de la lista se hacen operaciones logarítmicas (assocM y lookupM)

-- mergeMaps queda con costo O(k + k log k), siendo k la cantidad de claves del map, porque por cada clave del map se
-- hace una operación logarítmica (lookupM) y además el domM que se suma tiene costo lineal sobre las claves.

-- Ejercicio 5
-- Como usuario del tipo Empresa implementar las siguientes operaciones, calculando el costo obtenido al implementarlas, y justifi cando cada uno adecuadamente.

-- Propósito: construye una empresa con la información de empleados dada. Los sectores no tienen empleados. Costo: calcular.
comenzarCon :: [SectorId] -> [CUIL] -> Empresa

-- Propósito: dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes). Costo: calcular.
recorteDePersonal :: Empresa -> Empresa

-- Propósito: dado un CUIL de empleado le asigna todos los sectores de la empresa. Costo: calcular.
convertirEnComodin :: CUIL -> Empresa -> Empresa

-- Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores. Costo: calcular.
esComodin :: CUIL -> Empresa -> Bool