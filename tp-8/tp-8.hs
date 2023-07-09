-- USUARIO DEL TAD ORGANIZADOR
-- Interfaz:
-- nuevo :: Organizador
-- Propósito: Un organizador vacío. Eficiencia: O(1)

-- agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
-- Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
-- de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
-- no está vacío.
-- Eficiencia: no hay ninguna garantía de eficiencia.

-- todosLosProgramas :: Organizador -> [Checksum]
-- Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.

-- autoresDe :: Organizador -> Checksum -> Set Persona
-- Propósito: denota el conjunto de autores que aparecen en un programa determinado.
-- Precondición: el Checksum debe corresponder a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.

-- programasDe :: Organizador -> Persona -> Set Checksum
-- Propósito: denota el conjunto de programas en los que participó una determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.

-- programaronJuntas :: Organizador -> Persona -> Persona -> Bool
-- Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
-- programas del organizador, y C la cantidad total de programas.

-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
-- programaron juntas.
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum

-- Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
esUnGranHacker :: Organizador -> Persona -> Bool

-- USUARIO TAD NAVE

-- Propósito: Denota los tripulantes de la nave.
tripulantes :: Nave -> Set Tripulante

-- Propósito: Elimina al tripulante de la nave.
-- Pista: Considere reconstruir la nave sin ese tripulante.
bajaDeTripulante :: Tripulante -> Nave -> Nave
bajaDeTripulante t n = let ss = sectores n
                        in naveSinT t ss n (naveVacia ss)

naveSinT :: Tripulante -> [Sector] -> Nave -> Nave -> Nave
naveSinT t []     n n' = n'
naveSinT t (s:ss) n n' = let ts = tripulantesDe s n
                          in agregarTripulantes (set2list(removeS t ts)) s (naveSinT t ss n n')

agregarTripulantes :: [Tripulante] -> Sector -> Nave -> Nave
agregarTripulantes []     s n = n
agregarTripulantes (t:ts) s n = agregarTripulante t s (agregarTripulantes ts s n)