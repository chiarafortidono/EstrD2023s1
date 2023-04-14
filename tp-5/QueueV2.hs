module QueueV2 (QueueV2, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

-- Queue (cola)
-- Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa que los elementos salen en el orden con el 
-- que entraron, es decir, el que se agrega primero es el primero en salir (como la cola de un banco). Su interfaz es la siguiente:

-- Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare la eficiencia entre ambas implementaciones.
data QueueV2 a = ...

-- Crea una cola vacía.
emptyQ :: Queue a
-- Dada una cola indica si la cola está vacía.
isEmptyQ :: Queue a -> Bool
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue :: a -> Queue a -> Queue a
-- Dada una cola devuelve el primer elemento de la cola.
firstQ :: Queue a -> a
-- Dada una cola la devuelve sin su primer elemento.
dequeue :: Queue a -> Queue a

-- Me faltan los costos
emptyQ             = Q2 []
isEmptyQ   (Q2 es) = null es
enqueue  e (Q2 es) = Q2 (e:es)
firstQ     (Q2 es) = elUltimo es
dequeue    (Q2 es) = Q2 (sinElUltimo es)

elUltimo :: [a] -> a
elUltimo []     = error "No hay elementos en la queue"
elUltimo (x:xs) = if null xs
                    then x
                    else elUltimo xs

sinElUltimo :: [a] -> [a]
sinElUltimo []     = []
sinElUltimo (x:xs) = if null xs
                        then [x]
                        else x : sinElUltimo xs