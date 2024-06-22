module Tarea4 where

{--
    Nombre: Leandro Gugiato
    Numero: 318621
    --------------------------
    Nombre: Ruben Derigo
    Numero: 287176
--}

--Grafo ponderado:
type Ciudad = String --Vertices
type Costo = Int --Ponderacion de las aristas
type MapaDeVuelos = [(Ciudad, [(Ciudad, Costo)])] --Grafo ponderado

--Estructuras auxiliares:
type Visitadas = [Ciudad]
type Costos = [(Ciudad, Costo)]

-- Para representar el infinito
inf :: Int
inf = 9999 

--Ejercicio 1:
mapadevuelos :: MapaDeVuelos
mapadevuelos = [
  ("Newcastle", [("Leeds", 4), ("Manchester", 2)]),
  ("Leeds", [("Liverpool", 2), ("Sheffield", 4)]),
  ("Manchester", [("Liverpool", 4), ("Leeds", 1), ("Sheffield", 6)]),
  ("Liverpool", [("Sheffield", 1)]),
  ("Sheffield", [])
  ]

--Ejercicio 2:
ciudades :: MapaDeVuelos -> [Ciudad]
ciudades [] = []
ciudades ((c,ady):mdv) = c:(ciudades mdv)


--Ejercicio 3:
initListaCostos :: [Ciudad] -> Costos
initListaCostos [] = []
initListaCostos (x:xs) = (x,inf):(initListaCostos xs)


--Ejercicio 4:
costoCiudad :: Ciudad -> Costos -> Costo
costoCiudad ci [] = error "No se encontro la ciudad"
costoCiudad ci ((ciudad', co):costs)
  | ci == ciudad' = co
  | otherwise = costoCiudad ci costs


--Ejercicio 5:
actualizarCosto :: Costos -> Ciudad -> Costo -> Costos
actualizarCosto [] ci co = [(ci, co)]
actualizarCosto ((ciudad',costo'):costs) ci co
  | (ciudad' == ci) && (co < costo') = (ciudad',co):costs
  | otherwise = (ciudad',costo'):(actualizarCosto costs ci co)


--Ejercicio 6:
obtenerAdyacentes :: Ciudad -> MapaDeVuelos -> (Ciudad, [(Ciudad,Costo)])
obtenerAdyacentes c [] = error "Ciudad no encontrada"
obtenerAdyacentes c ((v,ady):mvd)
  | c == v = (v,ady)
  | otherwise = obtenerAdyacentes c mvd


--Ejercicio 7:
actualizarCostoAdyacentes :: (Ciudad,[(Ciudad,Costo)]) -> Costos -> Costos
actualizarCostoAdyacentes (c,[]) costs = costs
actualizarCostoAdyacentes (c,((ci,co):xs)) costs
  | ((costoCiudad c costs) + co) < (costoCiudad ci costs) = actualizarCostoAdyacentes (c,xs) (actualizarCosto costs ci (costoCiudad c costs + co))
  | otherwise = actualizarCostoAdyacentes (c,xs) costs


--Ejercicio 8:
obtenerCiudadesNoVisitadas:: Costos -> Visitadas -> Costos
obtenerCiudadesNoVisitadas [] vs = []
obtenerCiudadesNoVisitadas ((ci,co):costs) vs
  | elem ci vs = obtenerCiudadesNoVisitadas costs vs
  | otherwise = (ci,co):(obtenerCiudadesNoVisitadas costs vs)

ciudadMenorCosto :: Costos -> Ciudad
ciudadMenorCosto [] = error "No hay costos"
ciudadMenorCosto [(ci,co)] = ci
ciudadMenorCosto ((ci1,co1):((ci2,co2):costs))
  | co1 <= co2 = ciudadMenorCosto ((ci1,co1):costs)
  | otherwise = ciudadMenorCosto ((ci2,co2):costs)

ciudadConMenorCosto :: Costos -> Visitadas -> Ciudad
ciudadConMenorCosto costs visited
  | (length costs) == (length visited) = error "Todas las ciudades fueron visitadas"
  | otherwise = ciudadMenorCosto (obtenerCiudadesNoVisitadas costs visited)
 

-------------------------
--Algoritmo de Dijkstra--
-------------------------

--Ejercicio 9:
menorCosto :: MapaDeVuelos -> Ciudad -> Ciudad -> Costo
menorCosto mdv o d = costoCiudad d (visitarlasTodas (actualizarCosto (initListaCostos (ciudades mdv)) o 0) [] mdv)
--Ejemplo:  menorCosto mapadevuelos "Newcastle" "Sheffield" = 6

--Auxiliar de menorCosto para recorrer todas las ciudades.
visitarlasTodas :: Costos -> Visitadas -> MapaDeVuelos -> Costos
visitarlasTodas c v mdv
    | (length v) == (length (ciudades mdv)) = c
    | otherwise = visitarlasTodas (actualizarCostoAdyacentes (obtenerAdyacentes (ciudadConMenorCosto c v) mdv) (actualizarCosto c (ciudadConMenorCosto c v) (costoCiudad (ciudadConMenorCosto c v) c))) ((ciudadConMenorCosto c v):v) mdv


-------
--FIN--
-------