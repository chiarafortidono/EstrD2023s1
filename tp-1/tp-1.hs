-- 2. Números enteros
-- 1. Defina las siguientes funciones:
-- a) Dado un número devuelve su sucesor
sucesor :: Int -> Int
sucesor n = n + 1

-- b) Dados dos números devuelve su suma utilizando la operación +.
sumar :: Int -> Int -> Int
sumar n m = n + m

-- c) Dado dos números, devuelve un par donde la primera componente es la división del primero por el segundo, y la segunda 
-- componente es el resto de dicha división. Nota: para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
-- provista por Haskell.
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto _ 0 = error "No se puede dividir por 0"
divisionYResto n m = (div n m, mod n m)

-- d) Dado un par de números devuelve el mayor de estos.
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if n > m 
                    then n
                    else m

-- 2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
-- Ejemplo: maxDelPar (divisionYResto (suma 5 5) (sucesor 0))
-- sucesor (maxDelPar (divisionYResto (sumar 17 1) 2))
-- sumar (sucesor (maxDelPar (8, 2))) (fst (divisionYResto 1 1))
-- maxDelPar (divisionYResto (sumar 11 (sucesor 8)) 2)
-- fst (divisionYResto (sumar 25 25) (sucesor (maxDelPar (4,1))))

-- 3. Tipos enumerativos
-- 1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar las siguientes funciones:
data Dir = Norte | Este | Sur | Oeste
    deriving Show

-- a) Dada una dirección devuelve su opuesta.
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este  = Oeste
opuesto Sur   = Norte
opuesto Oeste = Este

-- b) Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur     = True
iguales Este Este   = True
iguales Oeste Oeste = True
iguales _ _         = False

-- c) Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe la siguiente dirección a Oeste. 
-- Posee una precondición esta función? Es una función total o parcial? Por qué?
siguiente :: Dir -> Dir -- PRECOND.: la dirección Oeste no tiene siguiente
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste

-- 2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves, Viernes, Sabado y Domingo. 
-- Supongamos que el primer día de la semana es lunes, y el último es domingo. Luego implementar las siguientes funciones:
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
    deriving Show

-- a) Devuelve un par donde la primera componente es el primer día de la semana, y la segunda componente es el último día de 
-- la semana. Considerar definir subtareas útiles que puedan servir después.
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

-- b) Dado un día de la semana indica si comienza con la letra M.
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False

-- c) Dado dos días de semana, indica si el primero viene después que el segundo. Analizar la calidad de la solución respecto de 
-- la cantidad de casos analizados (entre los casos analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = codigoDia d1 > codigoDia d2

codigoDia :: DiaDeSemana -> Int
codigoDia Lunes     = 1
codigoDia Martes    = 2
codigoDia Miercoles = 3
codigoDia Jueves    = 4
codigoDia Viernes   = 5
codigoDia Sabado    = 6
codigoDia Domingo   = 7

-- d) Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio _       = True

-- 3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina las siguientes funciones utilizando 
-- pattern matching (no usar las funciones sobre booleanos ya definidas en Haskell):
-- a) Dado un booleano, si es True devuelve False, y si es False devuelve True. En Haskell ya está definida como not.
negar :: Bool -> Bool
negar True  = False
negar False = True

-- b) Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino devuelve True. Esta función debe ser 
-- tal que implica False (error "Mal") devuelva True. Nota: no viene implementada en Haskell.
implica :: Bool -> Bool -> Bool
implica False _ = True
implica True  b = b

-- c) Dados dos booleanos si ambos son True devuelve True, sino devuelve False. Esta función debe ser tal que yTambien False (error 
-- "Mal") devuelva False. En Haskell ya está definida como \&\&.
yTambien :: Bool -> Bool -> Bool
yTambien True  b = b
yTambien False _ = False

-- d) Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False. Esta función debe ser tal que oBien 
-- True (error "Mal") devuelva True. En Haskell ya está definida como ||.
oBien :: Bool -> Bool -> Bool
oBien False b = b
oBien True  _ = True

-- 4. Registros
-- 1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:
data Persona = P String Int
               --nombre edad

-- Devuelve el nombre de una persona
nombre :: Persona -> String
nombre (P n e) = n

-- Devuelve la edad de una persona
edad :: Persona -> Int
edad (P n e) = e

-- Aumenta en uno la edad de la persona.
crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

-- Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre s (P _ e) = P s e

-- Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

-- Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
                        then p1
                        else p2

-- 2. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un porcentaje de energía; y Entrenador, 
-- como un nombre y dos Pokémon. Luego definir las siguientes funciones:
data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Pokemon = Pk TipoDePokemon Int
    deriving Show
               --tipo          energia

data Entrenador = E String Pokemon Pokemon
    deriving Show
                  --nombre

-- Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera a fuego, fuego a planta y planta 
-- a agua. Y cualquier otro caso es falso.
superaA :: Pokemon -> Pokemon -> Bool
superaA pk1 pk2 = esSuperiorA (tipo pk1) (tipo pk2)

tipo :: Pokemon -> TipoDePokemon
tipo (Pk t _) = t

esSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperiorA Agua   Fuego  = True
esSuperiorA Fuego  Planta = True
esSuperiorA Planta Agua   = True
esSuperiorA _      _      = False

-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t e = unoSiCeroSino (mismoTipo t (tipo (primerPokemonDe e))) + unoSiCeroSino (mismoTipo t (tipo (segundoPokemonDe e)))

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True  = 1
unoSiCeroSino False = 0

primerPokemonDe :: Entrenador -> Pokemon
primerPokemonDe (E _ pk1 _) = pk1

segundoPokemonDe :: Entrenador -> Pokemon
segundoPokemonDe (E _ _ pk2) = pk2

mismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipo Fuego  Fuego = True
mismoTipo Agua   Agua  = True
mismoTipo Planta Planta = True
mismoTipo _      _      = False

-- Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = pokemonDe e1 ++ pokemonDe e2

pokemonDe :: Entrenador -> [Pokemon]
pokemonDe (E _ pk1 pk2) = [pk1, pk2]

-- 5. Funciones polimórficas
-- 1. Defina las siguientes funciones polimórficas:
-- a) Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo :: a -> a
loMismo x = x

-- b) Dado un elemento de algún tipo devuelve el número 7.
siempreSiete :: a -> Int
siempreSiete x = 7

-- c) Dadas una tupla, invierte sus componentes. Por qué existen dos variables de tipo diferentes? Hay dos variables de tipo diferentes porque no 
-- necesariamente el par esté formado por elementos del mismo tipo.
swap :: (a,b) -> (b, a)
swap (x, y) = (y, x)

-- 2. Responda la siguiente pregunta: Por qué estas funciones son polimórficas? Son polimórficas porque toman argumentos de cualquier tipo.

-- 6. Pattern matching sobre listas
-- 1. Defina las siguientes funciones polimórficas utilizando pattern matching sobre listas (no utilizar las funciones que ya 
-- vienen con Haskell):
-- a) Dada una lista de elementos, si es vacía devuelve True, sino devuelve False. Definida en Haskell como null.
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False

-- b) Dada una lista devuelve su primer elemento. Definida en Haskell como head. Nota: tener en cuenta que el constructor de 
-- listas es :.
elPrimero :: [a] -> a -- PRECOND.: La lista no puede ser vacía.
elPrimero []    = error "La lista no tiene elementos"
elPrimero (x:_) = x

-- c) Dada una lista devuelve esa lista menos el primer elemento. Definida en Haskell como tail. Nota: tener en cuenta que el 
-- constructor de listas es :.
sinElPrimero :: [a] -> [a] -- PRECOND.: La lista no puede ser vacía.
sinElPrimero []     = []
sinElPrimero (_:xs) = xs

-- d) Dada una lista devuelve un par, donde la primera componente es el primer elemento de la lista, y la segunda componente 
-- es esa lista pero sin el primero. Nota: tener en cuenta que el constructor de listas es :.
splitHead :: [a] -> (a, [a]) -- PRECOND.: La lista no puede ser vacía.
splitHead []     = error "La lista no tiene elementos"
splitHead (x:xs) = (x, xs)