module QueueV2 (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

-- Queue (cola)
-- Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa que los elementos salen en el orden con el 
-- que entraron, es decir, el que se agrega primero es el primero en salir (como la cola de un banco). Su interfaz es la siguiente:

-- Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare la eficiencia entre ambas implementaciones.

data Queue a = QQ [a]

-- Crea una cola vacía.
emptyQ :: Queue a
-- Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue :: a -> Queue a -> Queue a
-- Dada una cola devuelve el primer elemento de la cola. PRECOND.: la cola no debe estar vacía.
firstQ :: Queue a -> a
-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a

emptyQ             = QQ []                  -- Costo O(1)
isEmptyQ   (QQ es) = null es                -- Costo O(1)
enqueue  e (QQ es) = QQ (e:es)              -- Costo O(1)
firstQ     (QQ es) = elUltimo es            -- Costo O(n)
dequeue    (QQ es) = QQ (sinElUltimo es)    -- Costo O(n)

elUltimo :: [a] -> a -- Costo O(n)
elUltimo [x]    = x
elUltimo (x:xs) = if null xs
                    then x
                    else elUltimo xs

sinElUltimo :: [a] -> [a] -- Costo O(n)
sinElUltimo []     = []
sinElUltimo (x:[]) = []
sinElUltimo (x:xs) = if null xs
                        then [x]
                        else x : sinElUltimo xs