module QueueV3 (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

-- Queue (cola)
-- Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa que los elementos salen en el orden con el 
-- que entraron, es decir, el que se agrega primero es el primero en salir (como la cola de un banco). Su interfaz es la siguiente:

-- Queue con dos listas: implemente la interfaz de Queue pero en lugar de una lista utilice dos listas. Esto permitirá que todas las operaciones 
-- sean constantes (aunque alguna/s de forma amortizada). La estructura funciona de la siguiente manera. Llamemos a una de las listas fs 
-- (front stack) y a la otra bs (back stack). Quitaremos elementos a través de fs y agregaremos a través de bs, pero todas las operaciones deben 
-- garantizar el siguiente invariante de representación: Si fs se encuentra vacía, entonces la cola se encuentra vacía. Qué ventaja tiene esta 
-- representación de Queue con respecto a la que usa una sola lista?

data Queue a = QQQ [a] [a]
{- INV. REP.: en QQQ fs bs,
    *
-}

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