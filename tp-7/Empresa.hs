-- Ejercicio 4
-- Dado la siguiente representación para el tipo abstracto Empresa:

type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)
{- INV. REP.: en (ConsE mse mce),
    * Todos los CUILs de los empleados en mce, están al menos una vez en el set del mse.
    *
-}

-- Donde se observa que:
-- * los empleados son un tipo abstracto.
-- * el primer map relaciona id de sectores con los empleados que trabajan en dicho sector.
-- * el segundo map relaciona empleados con su número de CUIL.
-- * un empleado puede estar asignado a más de un sector
-- Tanto Map como Set exponen una interfaz eficiente con costos logarítmicos para inserción, búsqueda y borrado, 
-- tal cual vimos en clase.

-- Y sabemos que la interfaz de Empleado es:

-- consEmpleado     :: CUIL -> Empleado 
-- Propósito: construye un empleado con dicho CUIL. Costo: O(1)

-- cuil             :: Empleado -> CUIL 
-- Propósito: indica el CUIL de un empleado. Costo: O(1)

-- incorporarSector :: SectorId -> Empleado -> Empleado 
-- Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado. 
-- Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.

-- sectores         :: Empleado -> [SectorId] 
-- Propósito: indica los sectores en los que el empleado trabaja. Costo: O(1)

-- Dicho esto, indicar invariantes de representación adecuados para la estructura y definir la siguiente interfaz de 
-- Empresa, respetando los costos dados y calculando los faltantes. Justificar todos los costos dados. En los costos, 
-- S es la cantidad de sectores de la empresa, y E es la cantidad de empleados.

-- Propósito: construye una empresa vacía. Costo: O(1)
consEmpresa :: Empresa
consEmpresa = ConsE EmptyM EmptyM

-- Propósito: devuelve el empleado con dicho CUIL. Costo: O(log E) --> se deriva del costo de buscarPorCUILM
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE mse mce) = buscarPorCUILM c mce

-- O(log E), porque el costo de buscar al CUIL/empleado en el map es logarítmico.
buscarPorCUILM :: CUIL -> Map CUIL Empleado -> Empleado
buscarPorCUILM c mce = case (lookupM c mce) of
                        Just e  -> e
                        Nothing -> error "El empleado no pertenece a la empresa"

-- Propósito: indica los empleados que trabajan en un sector dado. Costo: O(logS + E)
-- El costo del lookup es O(log S) y el costo de pasar ese set a una lista es lineal sobre la cantidad de empleados del set, entonces O(E)
-- Por lo tanto, el costo final es O(log S) + O(E)
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
empleadosDelSector id (ConsE mse mce) = setToList (lookupM id mse)

-- Propósito: indica todos los CUIL de empleados de la empresa. Costo: O(E)
-- El costo de domM en el map que relaciona cada CUIL con el empleado es O(E), siendo E la cantidad de CUILs o Empleados en el map.
todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE mse mce) = domM mce

-- Propósito: indica todos los sectores de la empresa. Costo: O(S)
-- El costo de domM en el map que relaciona cada SectorId con el Set de empleados es O(S), siendo S cada uno de los ids de sector.
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE mse mce) = domM mse

-- Propósito: agrega un sector a la empresa, inicialmente sin empleados. Costo: O(logS)
-- El costo de assocM es O(log S), siendo S la cantidad de sectores del map que asocia SectorId con sets de empleados.
agregarSector :: SectorId -> Empresa -> Empresa
agregarSector id (ConsE mse mce) = ConsE (assocM id emptyS mse) mce

-- Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá el CUIL dado. Costo: calcular.
-- El costo de abrir la estructura es O(1), el costo de consEmpleado es O(1), el costo de crear la estructura es O(1), el costo de
-- agregarEmpleadoASectores es O(log S + E), el costo de assocM es O(log K), siendo K las keys del map, en este caso O(log E)
-- O(1) * 3 + O(log S + E) + O(log E) = O(log S + E + log E) --> no sé si E se come a log E
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado ids c (ConsE mse mce) = let e = consEmpleado c
                                         in ConsE (agregarEmpleadoASectores ids e mse) (assocM c e mce)

-- El costo de abrir la estructura de la lista es O(1), el del lookupM es O(log S), siendo S los sectores asociados en el map (las keys),
-- el de assocM es O(log S), el de addS es O(N), siendo N los elementos del set, o sea O(E).
-- O(1) + O(log S) * 2 + O(E) = O(log S + E)
agregarEmpleadoASectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmpleadoASectores []       e mse = mse
agregarEmpleadoASectores (id:ids) e mse = case (lookupM id mse) of
                                            Nothing -> assocM id (addS e emptyS) mse
                                            Just se -> assocM id (addS e se)     mse

-- Propósito: agrega un sector al empleado con dicho CUIL. Costo: calcular.
-- El costo de abrir la estructura es O(1), el costo del lookup es O(log K) siendo K las keys del map o sea O(log E), el costo de incorporarSector
-- es O(log S) siendo S la cantidad de sectores del empleado, el costo de construir la estructura es O(1), el costo de agregarEmpleadoASectores
-- es O(log S + E), el costo de assocM es O(log E) y el costo de deleteM es O(log E).
-- O(1) * 2 + O(log E) * 3 + O(log S) + O(log S + E) => O(log E) + O(log S) + O(logS + E) -> O(log S + E + log E) --> no sé si E se come a log E
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector id c (ConsE mse mce) = case (lookupM c mce) of
                                        Nothing -> error "El empleado con ese CUIL no trabaja en la empresa"
                                        Just e  -> let newE = incorporarSector id e
                                                    in ConsE (agregarEmpleadoASectores [id] newE mse) (assocM c newE (deleteM c mce))

-- Propósito: elimina al empleado que posee dicho CUIL. Costo: calcular.
-- El costo de abrir la estructura es O(1), el costo del lookupM es logarítmico sobre las keys del map o sea O(log E), el costo de crear
-- la estructura es O(1), el costo de sectores es O(1), el costo de borrarEmpleadosDeSectores es O(S * (log S + E)) y el costo de deleteM
-- es logarítmico sobre las keys del map o sea (log E)
-- O(1) * 3 + O(log E) * 2 + O(S * (E + log S)) => O(log E + (S * (E + log S)))
borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado c (ConsE mse mce) = case (lookupM c mce) of
                                    Nothing -> error "El empleado con ese CUIL no trabaja en la empresa"
                                    Just e  -> consE (borrarEmpleadoDeSectores e (sectores e) mse) (deleteM c mce)

-- El costo de abrir la estructura de la lista es O(1), el costo de removeS es lineal sobre la cantidad de elementos del set o sea O(E),
-- el costo de fromJust es O(1), el costo de lookupM es logarítmico sobre las keys del map o sea O(log S) y el lookup y el removeS se hacen
-- sobre cada sector de la lista.
-- O(1) + O(S * (E + log S)) => O(S * (E + log S))
borrarEmpleadoDeSectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
borrarEmpleadoDeSectores e []       mse = mse
borrarEmpleadoDeSectores e (id:ids) mse = let newMap = removeS e (fromJust (lookupM id mse))
                                           in borrarEmpleadoDeSectores e ids newMap