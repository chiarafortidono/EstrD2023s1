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
                                 (Bifurcacion (Cofre [Tesoro]) (Fin (Cofre [Chatarra]))
                                                               (Fin (Cofre [Tesoro, Chatarra])))) 

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

caminoAlTesoro' :: Mapa -> [Dir] -- PRECOND.: debe existir un único tesoro en el camino.
caminoAlTesoro' (Fin c)               = []
caminoAlTesoro' (Bifurcacion c m1 m2) = if hayTesoroEnCofre c 
                                          then []
                                          else elegirMapaConTesoro m1 m2

elegirMapaConTesoro :: Mapa -> Mapa -> [Dir]
elegirMapaConTesoro m1 m2 = if hayTesoro m1
                               then Izq : caminoAlTesoro' m1
                               else Der : caminoAlTesoro' m2

-- Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c)               = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if length (caminoDeLaRamaMasLarga m1) > length (caminoDeLaRamaMasLarga m2)
                                                then Izq : caminoDeLaRamaMasLarga m1
                                                else Der : caminoDeLaRamaMasLarga m2

caminoMasLargo :: [Dir] -> [Dir] -> [Dir]
caminoMasLargo ds1 ds2 = if length ds1 > length ds2
                            then ds1
                            else ds2

-- Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)               = [objetos c]
tesorosPorNivel (Bifurcacion c m1 m2) = objetos c : (zipListas (tesorosPorNivel m1) (tesorosPorNivel m2))

objetos :: Cofre -> [Objeto]
objetos (Cofre os) = os

zipListas :: [[a]] -> [[a]] -> [[a]]
zipListas   xss      []     = xss
zipListas   []       yss    = yss
zipListas (xs:xss) (ys:yss) = (xs ++ ys) : zipListas xss yss

-- Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c)               = []
todosLosCaminos (Bifurcacion c m1 m2) = [] : (consDirATodas Izq (todosLosCaminos m1) ++ consDirATodas Der (todosLosCaminos m2))

consDirATodas :: Dir -> [[Dir]] -> [[Dir]]
consDirATodas d []       = [[d]]
consDirATodas d (ds:dss) = (d:ds) : consDirATodas d dss

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show

nave1 = N (NodeT sector1 ((NodeT sector2 EmptyT (NodeT sector3 EmptyT EmptyT))) (NodeT sector4 EmptyT EmptyT))
sector1 = S "1" [LanzaTorpedos] ["chiara", "lucia"]
sector2 = S "2" [(Motor 3), (Almacen [Comida, Oxigeno, Comida])] ["lucas", "martin"]
sector3 = S "3" [(Almacen [Combustible, Combustible])] ["maria", "juan", "chiara"]
sector4 = S "4" [(Motor 2), LanzaTorpedos, (Almacen [Oxigeno, Oxigeno])] ["maite", "carla", "matias", "chiara"]

-- Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [ SectorId ]
sectores (N ts) = sectoresT ts

sectoresT :: Tree Sector -> [SectorId]
sectoresT EmptyT          = []
sectoresT (NodeT s t1 t2) = sectorId s : (sectoresT t1 ++ sectoresT t2)

sectorId :: Sector -> SectorId
sectorId (S id cs ts) = id

-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. 
-- Nota: el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N ts) = poderDePropulsionT ts

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT EmptyT          = 0
poderDePropulsionT (NodeT s t1 t2) = poderDePropulsionS s + poderDePropulsionT t1 + poderDePropulsionT t2

poderDePropulsionS :: Sector -> Int
poderDePropulsionS (S id cs ts) = poderDePropulsionCs cs

poderDePropulsionCs :: [Componente] -> Int
poderDePropulsionCs []     = 0
poderDePropulsionCs (c:cs) = propulsionC c + poderDePropulsionCs cs

propulsionC :: Componente -> Int
propulsionC (Motor n) = n
propulsionC _         = 0

-- Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N ts) = barrilesT ts

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT          = []
barrilesT (NodeT s t1 t2) = barrilesS s ++ barrilesT t1 ++ barrilesT t2

barrilesS :: Sector -> [Barril]
barrilesS (S id cs ts) = barrilesCs cs

barrilesCs :: [Componente] -> [Barril]
barrilesCs []     = []
barrilesCs (c:cs) = (barrilesC c) ++ barrilesCs cs

barrilesC :: Componente -> [Barril]
barrilesC (Almacen bs) = bs
barrilesC _            = []

-- Propósito: Añade una lista de componentes a un sector de la nave. 
-- Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N ts) = N (agregarATreeSector cs id ts)

agregarATreeSector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarATreeSector cs id EmptyT          = EmptyT
agregarATreeSector cs id (NodeT s t1 t2) = if id == sectorId s
                                             then (NodeT (agregarCs cs s) t1 t2)
                                             else (NodeT s (agregarATreeSector cs id t1) (agregarATreeSector cs id t2))

agregarCs :: [Componente] -> Sector -> Sector
agregarCs ncs (S id cs ts) = S id (cs++ncs) ts

-- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave -- PRECOND.: los SectorId deben existir en la nave.
asignarTripulanteA t ids (N ts) = N (asignarTripulanteATS t ids ts)

asignarTripulanteATS :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteATS t ids EmptyT          = EmptyT
asignarTripulanteATS t ids (NodeT s t1 t2) = NodeT (asignarTripulanteASector t ids s) (asignarTripulanteATS t ids t1) (asignarTripulanteATS t ids t2)

asignarTripulanteASector :: Tripulante -> [SectorId] -> Sector -> Sector
asignarTripulanteASector t ids (S id cs ts) = if pertenece id ids
                                                 then S id cs (t:ts)
                                                 else S id cs ts

pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False
pertenece e (x:xs) = e == x || pertenece e xs

-- Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N ts) = sectoresAsignadosTS t ts

sectoresAsignadosTS :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosTS t EmptyT          = []
sectoresAsignadosTS t (NodeT s t1 t2) = singularSi (sectorId s) (estaAsignado t s) ++ sectoresAsignadosTS t t1 ++ sectoresAsignadosTS t t2

singularSi :: a -> Bool -> [a]
singularSi x True  = [x]
singularSi x False = []

estaAsignado :: Tripulante -> Sector -> Bool
estaAsignado t (S id cs ts) = pertenece t ts

-- Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]
tripulantes (N ts) = sinDuplicados (tripulantesTS ts)

tripulantesTS :: Tree Sector -> [Tripulante]
tripulantesTS EmptyT          = []
tripulantesTS (NodeT s t1 t2) = tripulantesS s ++ tripulantesTS t1 ++ tripulantesTS t2

tripulantesS :: Sector -> [Tripulante]
tripulantesS (S id cs ts) = ts

sinDuplicados :: [String] -> [String]
sinDuplicados []     = []
sinDuplicados (s:[]) = (s:[])
sinDuplicados (s:ss) = if pertenece s ss 
                        then sinDuplicados ss
                        else s : sinDuplicados ss

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre deriving Show
data Manada = M Lobo deriving Show

manada = M lobo1'
lobo1' = Cazador "jorge" ["conejo", "liebre", "alce"] lobo2' lobo3' lobo4'
lobo2' = Explorador "marta" ["montana", "lago", "rio"] lobo5' lobo6'
lobo3' = Cazador "alicia" ["alce", "mula", "venado"] lobo7 lobo8' lobo9'
lobo4' = Explorador "luis" ["playa", "lago"] lobo10' lobo11'
lobo5' = Cria "carlos"
lobo6' = Cria "mario"
lobo7' = Cria "roberto"
lobo8' = Cria "norberto"
lobo9' = Cria "magdalena"
lobo10' = Cria "ana"
lobo11' = Cria "karina"

-- Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean crías. Resolver las siguientes 
-- funciones utilizando recursión estructural sobre la estructura que corresponda en cada caso:
manada1 = M lobo1
lobo1 = Cazador "Uno" ["venado", "alce"] lobo2 lobo3 lobo4
lobo2 = Explorador "Dos" ["montana", "lago"] lobo5 lobo6
lobo3 = Cria "Tres"
lobo4 = Cria "Cuatro"
lobo5 = Explorador "Cinco" ["lago"] lobo7 lobo8
lobo6 = Cria "Seis"
lobo7 = Cria "Siete"
lobo8 = Cria "Ocho"
 
-- Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
buenaCaza :: Manada -> Bool
buenaCaza m = cantidadDeAlimento m > cantidadDeCrias m

cantidadDeAlimento :: Manada -> Int
cantidadDeAlimento (M lobo) = cantidadDeAlimentoL lobo

cantidadDeAlimentoL :: Lobo -> Int
cantidadDeAlimentoL (Cazador    n ps l1 l2 l3) = alimentoEn ps + cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2 + cantidadDeAlimentoL l3
cantidadDeAlimentoL (Explorador n ts l1 l2)    = cantidadDeAlimentoL l1 + cantidadDeAlimentoL l2
cantidadDeAlimentoL (Cria n)                   = 0

alimentoEn :: [Presa] -> Int
alimentoEn ps = length ps

cantidadDeCrias :: Manada -> Int
cantidadDeCrias (M lobo) = cantidadDeCriasL lobo

cantidadDeCriasL :: Lobo -> Int
cantidadDeCriasL (Cazador    n ps l1 l2 l3) = cantidadDeCriasL l1 + cantidadDeCriasL l2 + cantidadDeCriasL l3
cantidadDeCriasL (Explorador n ts l1 l2)    = cantidadDeCriasL l1 + cantidadDeCriasL l2
cantidadDeCriasL (Cria       n)             = 1

-- Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto con su cantidad de presas. 
-- Nota: se considera que los exploradores y crías tienen cero presas cazadas, y que podrían formar parte del resultado si es 
-- que no existen cazadores con más de cero presas.
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M lobo) = elAlfaL lobo

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador    n ps l1 l2 l3) = elegirEntre (n, alimentoEn ps) (elegirEntre (elAlfaL l1) ((elegirEntre (elAlfaL l2) (elAlfaL l3))))
elAlfaL (Explorador n ts l1 l2)    = elegirEntre (elAlfaL l1) (elegirEntre (elAlfaL l2) (n, 0))
elAlfaL (Cria       n)             = (n, 0)

elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (n1, c1) (n2, c2) = if c1 > c2
                                    then (n1, c1)
                                    else (n2, c2)

-- Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que pasaron por dicho territorio.
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M lobo) = losQueExploraronL t lobo

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cazador    n ps l1 l2 l3) = losQueExploraronL t l1 ++ losQueExploraronL t l2 ++ losQueExploraronL t l3
losQueExploraronL t (Explorador n ts l1 l2)    = singularSi n (pertenece t ts) ++ losQueExploraronL t l1 ++ losQueExploraronL t l2
losQueExploraronL t (Cria       n)             = []

-- Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la 
-- lista de los nombres de los exploradores que exploraron dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lobo) = exploradoresPorTerritorioL lobo

exploradoresPorTerritorioL :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioL (Cazador    n ps l1 l2 l3) = juntarExploradores (exploradoresPorTerritorioL l1) (juntarExploradores (exploradoresPorTerritorioL l2) (exploradoresPorTerritorioL l3))
exploradoresPorTerritorioL (Explorador n ts l1 l2)    = (crearTuplasExplorador n ts) ++ juntarExploradores (exploradoresPorTerritorioL l1) (exploradoresPorTerritorioL l2)
exploradoresPorTerritorioL (Cria       n)             = []

crearTuplasExplorador :: Nombre -> [Territorio] -> [(Territorio, [Nombre])]
crearTuplasExplorador n []     = []
crearTuplasExplorador n (t:ts) = (t, [n]) : crearTuplasExplorador n ts

juntarExploradores :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
juntarExploradores []       ys = ys
juntarExploradores (tn:tns) ys = agregarNombres tn (juntarExploradores tns ys)

agregarNombres :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
agregarNombres (t, ns) []              = [(t,ns)]
agregarNombres (t, ns) ((t', ns'):tns) = if t == t'
                                          then (t, ns++ns') : tns
                                          else (t', ns') : agregarNombres (t,ns) tns

-- Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los cazadores que tienen como subordinado al cazador dado 
-- (directa o indirectamente).
-- Precondición: hay un cazador con dicho nombre y es único.
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M lobo) = superioresDelCazadorL n lobo

superioresDelCazadorL :: Nombre -> Lobo -> [Nombre]
superioresDelCazadorL n (Cazador    n' ps l1 l2 l3) = if ((nombreLobo l1) == n) || ((nombreLobo l2) == n) || ((nombreLobo l3) == n)
                                                        then n' : []
                                                        else superioresDelCazadorL n l1 ++ superioresDelCazadorL n l2 ++ superioresDelCazadorL n l3
superioresDelCazadorL n (Explorador n' ts l1 l2)    = superioresDelCazadorL n l1 ++ superioresDelCazadorL n l2
superioresDelCazadorL n (Cria       n')             = []

nombreLobo :: Lobo -> Nombre
nombreLobo (Cazador    n ps l1 l2 l3) = n
nombreLobo (Explorador n ts l1 l2)    = n
nombreLobo (Cria       n)             = n