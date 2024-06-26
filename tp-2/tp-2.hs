hayAlMenosUnCinco :: [Int] -> Bool
hayAlMenosUnCinco []     = False
hayAlMenosUnCinco (n:ns) = n == 5 || hayAlMenosUnCinco ns

hayAlMenosUn :: Int -> [Int] -> Bool
hayAlMenosUn n []     = False
hayAlMenosUn n (x:xs) = n == x || hayAlMenosUn n xs

soloLosMayoresQue :: Int -> [Int] -> [Int]
soloLosMayoresQue n []     = []
soloLosMayoresQue n (x:xs) = if x > n
                                then x : soloLosMayoresQue n xs
                                else soloLosMayoresQue n xs

data Dir = Norte | Este | Sur | Oeste

iniciales :: [Dir] -> [Char]
iniciales []     = []
iniciales (d:ds) = inicial d : iniciales ds

inicial :: Dir -> Char
inicial Norte = 'N'
inicial Este  = 'E'
inicial Sur   = 'S'
inciial Oeste = 'O'

zip' :: [a] -> [b] -> [(a,b)]
zip' []     _      = []
zip' (x:xs) []     = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

replicar :: Int -> a -> [a]
replicar 0 x = []
replicar n x = x : replicar (n-1) x

cuentaRegresivaDesde :: Int -> [Int]
cuentaRegresivaDesde 0 = [0]
cuentaRegresivaDesde n = n : cuentaRegresivaDesde (n-1)

losPrimerosN :: Int -> [a] -> [a]
losPrimerosN _ []    = []
losPrimerosN 0 _     = []
losPrimerosN n (x:xs) = x : losPrimerosN (n-1) xs

-- 1. Recursión sobre listas
-- Defina las siguientes funciones utilizando recursión estructural sobre listas, salvo que se indique lo contrario:

-- 1) Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (n:ns) = n + sumatoria ns

-- 2) Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad de elementos que posee.
longitud :: [a] -> Int
longitud []     = 0
longitud (e:es) = 1 + longitud es

-- 3) Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (n:ns) = (n + 1) : sucesores ns

-- 4) Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (b:bs) = b && conjuncion bs

-- 5) Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (b:bs) = b || disyuncion bs

-- 6) Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]] -> [a]
aplanar []       = []
aplanar (xs:xss) = xs ++ aplanar xss

-- 7) Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False
pertenece e (x:xs) = e == x || pertenece e xs

-- 8) Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = if e == x
                        then 1 + apariciones e xs
                        else apariciones e xs

-- 9) Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int] -> [Int] 
losMenoresA n []     = []
losMenoresA n (x:xs) = if n > x
                        then x : losMenoresA n xs
                        else losMenoresA n xs

-- 10) Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []       = []
lasDeLongitudMayorA n (xs:xss) = if longitud xs > n
                                    then xs : lasDeLongitudMayorA n xss
                                    else lasDeLongitudMayorA n xss

-- 11) Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     e = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

-- 12) Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los elementos de la segunda a continuación. 
-- Definida en Haskell como (++).
agregar :: [a] -> [a] -> [a]
agregar []     ys = ys
agregar (x:xs) ys = x : agregar xs ys

-- 13) Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Definida en Haskell como reverse.
reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = reversa xs ++ [x]

-- 14) Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el máximo entre el elemento n de la primera 
-- lista y de la segunda lista, teniendo en cuenta que las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs     []    = xs
zipMaximos []     ys    = ys
zipMaximos (x:xs) (y:ys) = if x > y
                            then x : zipMaximos xs ys
                            else y : zipMaximos xs ys

-- 15) Dada una lista devuelve el mínimo.
elMinimo :: Ord a => [a] -> a -- PRECOND.: la lista no puede ser vacia.
elMinimo [x]    = x
elMinimo (x:xs) = if x < elMinimo xs
                    then x
                    else elMinimo xs

-- 2. Recursión sobre números
-- Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique lo contrario:
-- 1. Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta llegar a 0. Si n es 0 devuelve 1. 
-- La función es parcial si n es negativo.
factorial :: Int -> Int -- PRECOND.: n >= 0
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 2. Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre n y 1 (incluidos). Si el número es 
-- inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

-- 3. Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 x = []
repetir n x = x : repetir (n-1) x

-- 4. Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs. Si la lista es vacía, devuelve una 
-- lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _      = []
losPrimeros n []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

-- 5. Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista recibida. Si n es cero, devuelve la 
-- lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ []     = []
sinLosPrimeros 0 xs     = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

-- 3. Registros
-- 1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:

data Persona = P String Int deriving Show
            --   nombre edad

chiara = P "Chiara" 22
lucia  = P "Lucia"  22
camila = P "Camila" 21
malena = P "Malena" 16
lucas  = P "Lucas"  30

-- Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA e []     = []
mayoresA e (p:ps) = if edad p > e 
                        then p : mayoresA e ps
                        else mayoresA e ps

edad :: Persona -> Int
edad (P _ e) = e

-- Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
promedioEdad :: [Persona] -> Int -- PRECOND.: la lista posee al menor una persona.
promedioEdad ps = div (sumatoria (edades ps)) (longitud ps)

edades :: [Persona] -> [Int]
edades []     = []
edades (p:ps) = edad p : edades ps

-- Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona -- PRECOND.: la lista posee al menor una persona.
elMasViejo [p]    = p
elMasViejo (p:ps) = if edad p > edad (elMasViejo ps)
                        then p
                        else elMasViejo ps

-- 2. Modificaremos la representación de Entrenador y Pokemon de la práctica anterior de la siguiente manera:

data TipoDePokemon = Agua | Fuego | Planta        deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int      deriving Show
                        -- tipo          energia
data Entrenador = ConsEntrenador String [Pokemon] deriving Show
                              -- nombre pokemones

bulbasaur  = ConsPokemon Planta 100
psyduck    = ConsPokemon Agua   90
vileplume  = ConsPokemon Planta 74
charmander = ConsPokemon Fuego  65
squirtle   = ConsPokemon Agua   94
oddish     = ConsPokemon Planta 85

chiaraE = ConsEntrenador "chiara" [bulbasaur, psyduck, vileplume, squirtle, charmander]
luciaE  = ConsEntrenador "lucia"  []

-- Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria. Definir en base a esa representación las siguientes funciones:

-- Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon :: Entrenador -> Int
cantPokemon e = longitud (pokemonesDe e)

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (ConsEntrenador _ ps) = ps

-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe t e = longitud (pokemonesDeTipo t (pokemonesDe e))

pokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDeTipo t []     = []
pokemonesDeTipo t (p:ps) = if mismoTipo t (tipo p)
                            then p : pokemonesDeTipo t ps
                            else pokemonesDeTipo t ps

-- tp-1
mismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipo Fuego  Fuego = True
mismoTipo Agua   Agua  = True
mismoTipo Planta Planta = True
mismoTipo _      _      = False

-- tp-1
tipo :: Pokemon -> TipoDePokemon
tipo (ConsPokemon t _) = t

-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían a los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 = longitud (losDeTipo_De_QueLeGananATodosLosDe_ t e1 e2)

losDeTipo_De_QueLeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> [Pokemon]
losDeTipo_De_QueLeGananATodosLosDe_ t e1 e2 = pokemonesDeTipo t (losQueLeGananATodos (pokemonesDe e1) (pokemonesDe e2))

losQueLeGananATodos :: [Pokemon] -> [Pokemon] -> [Pokemon]
losQueLeGananATodos []     _   = []
losQueLeGananATodos (p:ps) pks = if superaATodos p pks
                                    then p : losQueLeGananATodos ps pks
                                    else losQueLeGananATodos ps pks

superaATodos :: Pokemon -> [Pokemon] -> Bool
superaATodos p []       = True
superaATodos p (pk:pks) = superaA p pk && superaATodos p pks

-- tp-1
superaA :: Pokemon -> Pokemon -> Bool
superaA pk1 pk2 = esSuperiorA (tipo pk1) (tipo pk2)

-- tp-1
esSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperiorA Agua   Fuego  = True
esSuperiorA Fuego  Planta = True
esSuperiorA Planta Agua   = True
esSuperiorA _      _      = False

-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = existeUnoDeTipo Agua ps && existeUnoDeTipo Fuego ps && existeUnoDeTipo Planta ps

existeUnoDeTipo :: TipoDePokemon -> [Pokemon] -> Bool
existeUnoDeTipo t []     = False
existeUnoDeTipo t (p:ps) = mismoTipo t (tipo p) || existeUnoDeTipo t ps

-- 3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro de una empresa de software, junto al 
-- proyecto en el que se encuentran. Así, una empresa es una lista de personas con diferente rol. La definición es la siguiente:

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

proyectoA = ConsProyecto "ProyectoA"
proyectoB = ConsProyecto "ProyectoB"

carla   = Management Senior     proyectoB
marcos  = Management SemiSenior proyectoA
daniela = Developer  Senior     proyectoB
tomas   = Developer  Senior     proyectoB
martina = Developer  SemiSenior proyectoA

empresa = ConsEmpresa [marcos, martina, carla, daniela, tomas]

-- Definir las siguientes funciones sobre el tipo Empresa:

-- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = sinDuplicados (proyectosDeRoles rs)

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles []     = []
proyectosDeRoles (r:rs) = proyecto r : proyectosDeRoles rs

proyecto :: Rol -> Proyecto
proyecto (Developer  _ p) = p
proyecto (Management _ p) = p

mismoProyecto :: Proyecto -> Proyecto -> Bool
mismoProyecto p1 p2 = mismoNombre (nombreProyecto p1) (nombreProyecto p2)

mismoNombre :: String -> String -> Bool
mismoNombre s1 s2 = s1 == s2

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto s) = s

nombresProyectos :: [Proyecto] -> [String]
nombresProyectos []     = []
nombresProyectos (p:ps) = nombreProyecto p : nombresProyectos ps

sinDuplicados :: [Proyecto] -> [Proyecto]
sinDuplicados []     = []
sinDuplicados (p:ps) = if pertenece (nombreProyecto p) (nombresProyectos ps)
                        then sinDuplicados ps
                        else p : sinDuplicados ps

-- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertenecen además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa rs) ps = longitud (losDevSeniorDe_ (losQueParticipanEn_ rs ps) ps)

losDevSeniorDe_ :: [Rol] -> [Proyecto] -> [Rol]
losDevSeniorDe_ []     ps = []
losDevSeniorDe_ (r:rs) ps = if esDevSenior r
                                then r : losDevSeniorDe_ rs ps
                                else losDevSeniorDe_ rs ps
                          
losQueParticipanEn_ :: [Rol] -> [Proyecto] -> [Rol]
losQueParticipanEn_ []     ps = []
losQueParticipanEn_ (r:rs) ps = if participa_EnAlgunProyecto r ps
                                    then r : losQueParticipanEn_ rs ps
                                    else losQueParticipanEn_ rs ps

participa_EnAlgunProyecto :: Rol -> [Proyecto] -> Bool
participa_EnAlgunProyecto r []     = False
participa_EnAlgunProyecto r (p:ps) = mismoProyecto (proyecto r) p || participa_EnAlgunProyecto r ps

esDevSenior :: Rol -> Bool
esDevSenior (Developer Senior _) = True
esDevSenior _                    = False

-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) = longitud (losQueParticipanEn_ rs ps)

-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = asignadosDe_PorProyectos e (proyectos e)

asignadosDe_PorProyectos :: Empresa -> [Proyecto] -> [(Proyecto, Int)]
asignadosDe_PorProyectos e []     = []
asignadosDe_PorProyectos e (p:ps) = (p, (cantQueTrabajaEnProyecto (roles e) p)) : asignadosDe_PorProyectos e ps

cantQueTrabajaEnProyecto :: [Rol] -> Proyecto -> Int
cantQueTrabajaEnProyecto []     p = 0
cantQueTrabajaEnProyecto (r:rs) p = unoSiCeroSino (mismoProyecto (proyecto r) p) + cantQueTrabajaEnProyecto rs p

roles :: Empresa -> [Rol]
roles (ConsEmpresa rs) = rs

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True  = 1
unoSiCeroSino False = 0