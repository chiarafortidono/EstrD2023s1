data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

pizza0 = Prepizza
pizza1 = Capa Salsa Prepizza
pizza2 = Capa Queso pizza1
pizza3 = Capa (Aceitunas 10) pizza2
pizza4 = Capa Queso (Capa Salsa (Capa Queso Prepizza))
pizza5 = Capa Jamon pizza4

-- Definir las siguientes funciones:

-- Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza     = 0
cantidadDeCapas (Capa ing p) = 1 + cantidadDeCapas p

-- Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

-- Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza     = Prepizza
sacarJamon (Capa ing p) = if esJamon ing
                            then sacarJamon p
                            else Capa ing (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

-- Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En particular, la prepizza, al no 
-- tener ningún ingrediente, debería dar verdadero.)
-- salsa Y queso solamente o salsa O queso?
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza     = True
tieneSoloSalsaYQueso (Capa ing p) = (esSalsa ing || esQueso ing) && tieneSoloSalsaYQueso p

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _     = False

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

-- Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) = if sonAceitunas ing
                                    then Capa (duplicarCantAceitunas ing) (duplicarAceitunas p)
                                    else Capa ing (duplicarAceitunas p)

sonAceitunas :: Ingrediente -> Bool
sonAceitunas (Aceitunas n) = True
sonAceitunas _             = False

duplicarCantAceitunas :: Ingrediente -> Ingrediente
duplicarCantAceitunas (Aceitunas n) = Aceitunas (n*2)
duplicarCantAceitunas ing           = ing

-- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de ingredientes de la pizza, y la 
-- respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = ((cantidadDeCapas p), p) : cantCapasPorPizza ps

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

mapa0 = Fin (Cofre [Tesoro])

mapa1 = Bifurcacion (Cofre [Chatarra]) 
                    (Fin (Cofre [Chatarra]))
                    (Fin (Cofre [Tesoro]))

mapa2 = Bifurcacion (Cofre [Chatarra]) 
                    (Bifurcacion (Cofre [Chatarra]) 
                                 (Fin (Cofre [Chatarra])) 
                                 (Fin (Cofre [Chatarra]))) 
                    (Fin (Cofre [Tesoro]))

mapa3 = Bifurcacion (Cofre [Chatarra]) 
                    (Bifurcacion (Cofre [Chatarra]) 
                                 (Fin (Cofre [Chatarra])) 
                                 (Fin (Cofre [Chatarra]))) 
                    (Bifurcacion (Cofre [Chatarra]) 
                                 (Fin (Cofre [Chatarra])) 
                                 (Fin (Cofre [Tesoro]))) 

-- Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre os) = hayTesoroEnObjetos os

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (o:os) = esTesoro o || hayTesoroEnObjetos os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

-- Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn []     m = hayTesoroEnElLugarActual m
hayTesoroEn (d:ds) m = hayTesoroEn ds (avanzarHacia d m)

avanzarHacia :: Dir -> Mapa -> Mapa
avanzarHacia d (Fin c)               = Fin c
avanzarHacia d (Bifurcacion c m1 m2) = if esIzquierda d
                                        then m1
                                        else m2

hayTesoroEnElLugarActual :: Mapa -> Bool
hayTesoroEnElLugarActual (Fin c)               = hayTesoroEnCofre c
hayTesoroEnElLugarActual (Bifurcacion c m1 m2) = hayTesoroEnCofre c

esIzquierda :: Dir -> Bool
esIzquierda Izq = True
esIzquierda _   = False

-- Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir] -- PRECOND.: debe existir un único tesoro en el camino.
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroEnCofre c 
                                        then []
                                        else if hayTesoro m1
                                               then Izq : caminoAlTesoro m1
                                               else Der : caminoAlTesoro m2

-- Indica el camino de la rama más larga.
-- caminoDeLaRamaMasLarga :: Mapa -> [Dir]
-- caminoDeLaRamaMasLarga (Fin c)               = 
-- caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = 

-- Devuelve los tesoros separados por nivel en el árbol.
-- tesorosPorNivel :: Mapa -> [[Objeto]]

-- Devuelve todos lo caminos en el mapa.
-- todosLosCaminos :: Mapa -> [[Dir]]

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

-- Propósito: Devuelve todos los sectores de la nave.
-- sectores :: Nave -> [SectorId]

-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. 
-- Nota: el poder de propulsión es el número que acompaña al constructor de motores.
-- poderDePropulsion :: Nave -> Int

-- Propósito: Devuelve todos los barriles de la nave.
-- barriles :: Nave -> [Barril]

-- Propósito: Añade una lista de componentes a un sector de la nave. 
-- Nota: ese sector puede no existir, en cuyo caso no añade componentes.
-- agregarASector :: [Componente] -> SectorId -> Nave -> Nave

-- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.
-- asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave

-- Propósito: Devuelve los sectores en donde aparece un tripulante dado.
-- sectoresAsignados :: Tripulante -> Nave -> [SectorId]

-- Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
-- tripulantes :: Nave -> [Tripulante]

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre deriving Show
data Manada = M Lobo deriving Show

-- Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean crías. Resolver las siguientes 
-- funciones utilizando recursión estructural sobre la estructura que corresponda en cada caso:

-- Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
-- buenaCaza :: Manada -> Bool

-- Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto con su cantidad de presas. 
-- Nota: se considera que los exploradores y crías tienen cero presas cazadas, y que podrían formar parte del resultado si es 
-- que no existen cazadores con más de cero presas.
-- elAlfa :: Manada -> (Nombre, Int)

-- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que pasaron por dicho territorio.
-- losQueExploraron :: Territorio -> Manada -> [Nombre]

-- Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la 
-- lista de los nombres de los exploradores que exploraron dicho territorio. Los territorios no deben repetirse.
-- exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]

-- Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los cazadores que tienen como subordinado al 
-- cazador dado (directa o indirectamente).
-- Precondición: hay un cazador con dicho nombre y es único.
-- superioresDelCazador :: Nombre -> Manada -> [Nombre]