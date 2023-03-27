singularSi :: a -> Bool -> [a]
singularSi x True  = x:[]
singularSi x False = []

data Objeto' = Armadura | Escudo | Maza | Oro deriving Show

data Dungeon = Armario | Habitacion Objeto' Dungeon Dungeon deriving Show

profundidad :: Dungeon -> Int
profundidad Armario                = 0
profundidad (Habitacion obj d1 d2) = 1 + max (profundidad d1) (profundidad d2)

cambiarMazasPorOro :: Dungeon -> Dungeon
cambiarMazasPorOro Armario                = Armario
cambiarMazasPorOro (Habitacion obj d1 d2) = Habitacion (cambiarUnaMazaPorOro obj) (cambiarMazasPorOro d1) (cambiarMazasPorOro d2)

cambiarUnaMazaPorOro :: Objeto' -> Objeto'
cambiarUnaMazaPorOro Maza = Oro
cambiarUnaMazaPorOro obj  = obj

objetos :: Dungeon -> [Objeto']
objetos Armario                = []
objetos (Habitacion obj d1 d2) = obj : objetos d1 ++ objetos d2

objetosDelCaminoMasLargo :: Dungeon -> [Objeto']
objetosDelCaminoMasLargo Armario                = []
objetosDelCaminoMasLargo (Habitacion obj d1 d2) = obj : elegirCaminoMasLargo (objetosDelCaminoMasLargo d1) (objetosDelCaminoMasLargo d2)

elegirCaminoMasLargo :: [Objeto'] -> [Objeto'] -> [Objeto']
elegirCaminoMasLargo os1 os2 = if length os1 > length os2
                                then os1
                                else os2

data Tree' a = EmptyT' | NodeT' a (Tree' a) (Tree' a) deriving Show

-- Arma un dungeon donde cada nodo representa una Habitación y los árboles vacíos representan Armarios
armarDungeon :: Tree' Objeto' -> Dungeon
armarDungeon EmptyT'            = Armario
armarDungeon (NodeT' obj t1 t2) = Habitacion obj (armarDungeon t1) (armarDungeon t2)

-- 1. Tipos recursivos simples
-- 1.1. Celdas con bolitas
-- Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:

data Color = Azul | Rojo deriving Show

data Celda = Bolita Color Celda | CeldaVacia deriving Show

-- En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad de bolitas de ese color en la celda. 
-- Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría ser la siguiente:

celda0 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))

-- Implementar las siguientes funciones sobre celdas:

-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya existe una operación sobre listas que 
-- ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia     = 0
nroBolitas c (Bolita co ce) = unoSiCeroSino (mismoColor c co) + nroBolitas c ce

mismoColor :: Color -> Color -> Bool
mismoColor Azul Azul = True
mismoColor Rojo Rojo = True
mismoColor _    _    = False

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True  = 1
unoSiCeroSino False = 0

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner c ce = Bolita c ce

-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar c CeldaVacia     = CeldaVacia
sacar c (Bolita co ce) = if mismoColor c co
                            then ce
                            else Bolita co (sacar c ce)

-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c ce = ce
ponerN n c ce = poner c (ponerN (n-1) c ce)

-- 1.2. Camino hacia el tesoro
-- Tenemos los siguientes tipos de datos:

data Objeto = Cacharro | Tesoro deriving Show

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

camino0 = Cofre [Cacharro, Cacharro] Fin
camino1 = Nada (Cofre [Tesoro] (Nada Fin))

-- Definir las siguientes funciones:

-- Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino -> Bool
hayTesoro Fin          = False
hayTesoro (Cofre os c) = hayAlgunTesoroEn os || hayTesoro c
hayTesoro (Nada c)     = hayTesoro c

hayAlgunTesoroEn :: [Objeto] -> Bool
hayAlgunTesoroEn []     = False
hayAlgunTesoroEn (o:os) = esTesoro o || hayAlgunTesoroEn os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro. Si un cofre con un tesoro está al 
-- principio del camino, la cantidad de pasos a recorrer es 0. Precondición: tiene que haber al menos un tesoro.
-- pasosHastaTesoro :: Camino -> Int -- PRECOND.: hay al menos un tesoro en el camino.

-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de pasos es 5, indica si hay un 
-- tesoro en 5 pasos.
-- hayTesoroEn :: Int -> Camino -> Bool

-- Indica si hay al menos “n” tesoros en el camino.
-- alMenosNTesoros :: Int -> Camino -> Bool

-- (desafío) Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si el rango es 3 y 5, indica 
-- la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están incluidos tanto 3 como 5 en el resultado.
-- cantTesorosEntre :: Int -> Int -> Camino -> Int

-- 2. Tipos arbóreos
-- 2.1. Árboles binarios
-- Dada esta definición para árboles binarios:

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

-- Defina las siguientes funciones utilizando recursión estructural según corresponda:

-- 1. Dado un árbol binario de enteros devuelve la suma entre sus elementos.
-- sumarT :: Tree Int -> Int

-- 2. Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size en inglés). 
-- sizeT :: Tree a -> Int

-- 3. Dado un árbol de enteros devuelve un árbol con el doble de cada número.
-- mapDobleT :: Tree Int -> Tree Int

-- 4. Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el árbol.
-- perteneceT :: Eq a => a -> Tree a -> Bool

-- 5. Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son iguales a e.
-- aparicionesT :: Eq a => a -> Tree a -> Int

-- 6. Dado un árbol devuelve los elementos que se encuentran en sus hojas.
-- leaves :: Tree a -> [a]

-- 7. Dado un árbol devuelve su altura. Nota: la altura de un árbol (height en inglés), también llamada profundidad, es la cantidad
-- de niveles del árbol. La altura para EmptyT es 0, y para una hoja es 1.
-- heightT :: Tree a -> Int

-- 8. Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho, en cada nodo del árbol.
-- mirrorT :: Tree a -> Tree a

-- 9. Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order. Nota: En el modo in-order primero 
-- se procesan los elementos del hijo izquierdo, luego la raiz y luego los elementos del hijo derecho.
-- toList :: Tree a -> [a]

-- 10. Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un nodo es la distancia que hay de la raíz 
-- hasta él. La distancia de la raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1. Nota: El primer nivel de 
-- un árbol (su raíz) es 0.
-- levelN :: Int -> Tree a -> [a]

-- 11. Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de dicho árbol.
-- listPerLevel :: Tree a -> [[a]]

-- 12. Devuelve los elementos de la rama más larga del árbol.
-- ramaMasLarga :: Tree a -> [a]

-- 13. Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raiz hasta las hojas.
-- todosLosCaminos :: Tree a -> [[a]]

-- 2.2. Expresiones Aritméticas
-- El tipo algebraico ExpA modela expresiones aritméticas de la siguiente manera:

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA deriving Show

-- Implementar las siguientes funciones utilizando el esquema de recursión estructural sobre Exp:

-- 1. Dada una expresión aritmética devuelve el resultado evaluarla.
-- eval :: ExpA -> -> Int

-- 2. Dada una expresión aritmética, la simplifica según los siguientes criterios (descritos utilizando notación matemática 
-- convencional):
-- a) 0 + x = x + 0 = x
-- b) 0 * x = x * 0 = 0
-- c) 1 * x = x * 1 = x
-- d) - (- x) = x
-- simplificar :: ExpA -> ExpA