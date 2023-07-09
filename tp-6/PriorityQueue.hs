module PriorityQueue (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ) where

-- Siempre sale el mínimo elemento, es decir el de máxima prioridad.
-- Implementación con lista sin ordenar.
data PriorityQueue a = PQ [a]

emptyPQ     :: PriorityQueue a
isEmptyPQ   :: PriorityQueue a -> Bool
insertPQ    :: Ord a => a -> PriorityQueue a -> PriorityQueue a
findMinPQ   :: Ord a => PriorityQueue a -> a                        -- PRECOND.: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a          -- PRECOND.: parcial en caso de priority queue vacía.

emptyPQ               = PQ []               -- O(1)
isEmptyPQ     (PQ xs) = null xs             -- O(1)
insertPQ    x (PQ xs) = PQ (x:xs)           -- O(1)
findMinPQ     (PQ xs) = minimum xs          -- O(n)
deleteMinPQ   (PQ xs) = PQ (borrarMin xs)   -- O(n)

-- O(n)
borrarMin :: Ord a => [a] -> [a] -- PRECOND.: la lista no es vacía
borrarMin xs = borrar (minimum xs) xs

-- O(n)
borrar :: Eq a => a -> [a] -> [a]
borrar x []     = []
borrar x (y:ys) = if x == y
                    then ys
                    else y : borrar x ys