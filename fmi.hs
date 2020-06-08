import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
--import Data.Text.IsInfixOf
import Text.Show.Functions -- para que las funciones dentro del data se vean <function>
import Data.Eq
import Data.Char

data Pais = Pais {
    ipc :: Ipc,
    sectorPublico :: SectorPublico,
    sectorPrivado :: SectorPrivado,
    recursosNaturales :: [Recursos],
    deudaFmi :: Deuda
} deriving Show 

type Ipc = Float
type SectorPublico = Int
type SectorPrivado = Int
type Recursos = String
type Deuda = Float

namibia = Pais 4140 400000 650000 ["mineria","ecoturismo"] 50000000

--2 
type Estrategia = Pais -> Pais

prestamos :: Int -> Pais -> Pais
prestamos unosMillones unPais = modificarDeuda  (+ (fromIntegral (unosMillones * 150) / 100))  unPais

modificarDeuda :: (Int -> Int) -> Pais -> Pais
modificarDeuda funcion  unPais = unPais {deudaFmi = funcion (deudaFmi unPais)}

reducirPuestos :: Int -> Pais -> Pais
reducirPuestos unaCantidad unPais = (reduccionIpc unaCantidad .reduccionSectorPublico unaCantidad) unPais

reduccionIpc :: Int -> Pais -> Pais
reduccionIpc unaCantidad unPais | unaCantidad > 100 = modificarIpc 0.2 unPais 
                                | otherwise = modificarIpc 0.15 unPais
modificarIpc unPorcentaje unPais = unPais {ipc = (ipc unPais) * (1 - unPorcentaje) }

reduccionSectorPublico :: Int -> Pais -> Pais
reduccionSectorPublico unaCantidad unPais = unPais {sectorPublico = (sectorPublico unPais)- unaCantidad} 

darRecursos unRecurso unPais = (eliminarRecurso unRecurso.modificarDeuda (+(-200000))) unPais

eliminarRecurso unRecurso unPais = unPais {recursosNaturales = filter (/= unRecurso) (recursosNaturales unPais)}

blindaje :: Pais -> Pais
blindaje unPais = modificarDeuda (+ (calculoProductoBruto unPais)) unPais

calculoProductoBruto :: Pais -> Int
calculoProductoBruto unPais = ipc unPais * (sectorPublico unPais + sectorPrivado unPais - sectorPublico (reduccionSectorPublico 500 unPais))

-- 3

receta :: Receta
receta = [prestamos 200000 , eliminarRecurso "Mineria"]

aplicarReceta receta unPais = foldl1 unPais receta

-- 4
paisesQueZafan :: [Pais] -> [Pais]
paisesQueZafan unosPaises = filter (contieneRecurso "Petroleo") unosPaises

contieneRecurso unRecurso  unPais = elem unRecurso (recursosNaturales unPais)

deudaTotal unosPaises = sum (map deudaFmi unosPaises)

--5 
estaOrdenadoDePeorAmejorReceta _ unasRecetas = False
estaOrdenadoDePeorAmejorReceta unPais (unaReceta : otraReceta : demasRecetas) 
            | (calculoProductoBruto (unaReceta unPais) < calculoProductoBruto (otraReceta unPais)) = 