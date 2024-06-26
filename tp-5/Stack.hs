module Stack (Stack, emptySt, isEmptySt, push, top, pop, lenS) where

-- Stack (pila)
-- Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa que los últimos elementos agregados a la 
-- estructura son los primeros en salir (como en una pila de platos). Su interfaz es la siguiente:

-- Implementar el tipo abstracto Stack utilizando una lista.

data Stack a = St [a]

-- Crea una pila vacía.
emptySt :: Stack a
-- Dada una pila indica si está vacía.
isEmptySt :: Stack a -> Bool
-- Dados un elemento y una pila, agrega el elemento a la pila.
push :: a -> Stack a -> Stack a
-- Dada un pila devuelve el elemento del tope de la pila.
top :: Stack a -> a
-- Dada una pila devuelve la pila sin el primer elemento.
pop :: Stack a -> Stack a
-- Dada la cantidad de elementos en la pila. Costo: constante.
lenS :: Stack a -> Int

emptySt             = St []         -- Costo O(1)
isEmptySt   (St es) = null es       -- Costo O(1)
push      e (St es) = St (e:es)     -- Costo O(1)
top         (St es) = head es       -- Costo O(1)
pop         (St es) = St (tail es)  -- Costo O(1)
lenS        (St es) = length es     -- Costo O(1)