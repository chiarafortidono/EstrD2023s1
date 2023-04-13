module QueueV1 (QueueV1, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

-- Queue (cola)
-- Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa que los elementos salen en el orden con el 
-- que entraron, es decir, el que se agrega primero es el primero en salir (como la cola de un banco). Su interfaz es la siguiente:

-- Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el final de la lista y desencolarse por delante.
data QueueV1 a = ...

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