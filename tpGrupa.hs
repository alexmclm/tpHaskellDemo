import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
--import Data.Text.IsInfixOf
import Data.Maybe -- por si llegan a usar un metodo de coleccion y devuelva Nothing or justElements
import Text.Show.Functions -- para que las funciones dentro del data se vean <function>
import Data.Eq
import Data.List (sort)
import Data.Char


data Auto = Auto {
    patente :: Patente,
    desgasteLlantas :: [Desgaste],
    rpm :: Int,
    temperaturaAgua :: Int,
    ultimoArreglo :: Fecha
} deriving Show

type Patente = String
type Desgaste = Float
type Fecha = (Int,Int,Int)
type Persona = Auto -> Auto

anio :: Fecha -> Int
anio (_, _, year) = year

ford :: Auto
ford = Auto "AT001LN" [0.5, 0.1, 0.6, 0.4] 10000 100 (2,1,2016)

audi :: Auto
audi = Auto "DJV214" [0.51, 0.1, 0.6, 0.4] 2948 80 (3,10,2015)

chevrolet :: Auto
chevrolet = Auto "DJV215" [0.9, 0.9, 0.8, 0.4] 5893 63 (16,8,2013)

ferrari :: Auto
ferrari = Auto "DFH029" [0.8, 0.1, 0.6, 0.2] 9382 110 (13,5,2019)


-- PUNTO 1

-- costo reparacion de auto

costoReparacion :: Auto -> Int
costoReparacion unAuto | cantidadDigitosPatente unAuto == 7 = 12500
                       | patenteEstaEntreDJyNB unAuto = calculoParaPatentesPeculiar unAuto
                       | otherwise = 15000

cantidadDigitosPatente :: Auto -> Int
cantidadDigitosPatente unAuto = length (patente unAuto)

patenteEstaEntreDJyNB :: Auto -> Bool
patenteEstaEntreDJyNB unAuto = ((>= "DJ").take 2. patente) unAuto && ((<= "NB").take 2. patente) unAuto

patenteTerminaEnCuatro :: Auto -> Bool
patenteTerminaEnCuatro unAuto = ((=='4').last.patente) unAuto

calculoParaPatentesPeculiar :: Auto -> Int
calculoParaPatentesPeculiar unAuto | patenteTerminaEnCuatro unAuto = 3000 * cantidadDigitosPatente unAuto
                                   | otherwise = 20000

-- Punto 2

desgastePrimeraLlanta :: Auto -> Float
desgastePrimeraLlanta unAuto = (head.desgasteLlantas) unAuto

esPeligroso :: Auto -> Bool
esPeligroso unAuto = ((>0.5).desgastePrimeraLlanta) unAuto

anioRevision :: Auto -> Int
anioRevision unAuto = (anio.ultimoArreglo) unAuto

necesitaRevision :: Auto -> Bool
necesitaRevision unAuto = ((<=2015).anioRevision) unAuto

-- punto 3

--Alex
alfa :: Persona
alfa unAuto | (rpm unAuto) > 2000 = regularAuto 2000 unAuto
            | otherwise = id unAuto

regularAuto :: Int -> Auto -> Auto
regularAuto unaCantidad unAuto = unAuto {rpm = unaCantidad}

bravo :: Persona
bravo unAuto = unAuto {desgasteLlantas = [0, 0, 0, 0]}

charly :: Persona
charly unAuto = (alfa.bravo) unAuto

tango :: Persona
tango unAuto = unAuto

--Facu
revisarTemperaturaAgua :: Auto -> Auto
revisarTemperaturaAgua unAuto = unAuto {temperaturaAgua = 90}

zulu :: Persona
zulu unAuto = (lima.revisarTemperaturaAgua) unAuto

arreglarDosPrimerasLlantas :: [Desgaste] -> [Desgaste]
arreglarDosPrimerasLlantas [_, _, tercera, cuarta] = [0, 0, tercera, cuarta]
arreglarDosPrimerasLlantas _ = []

cambiarCubiertasDelanteras :: Auto -> Auto
cambiarCubiertasDelanteras unAuto = unAuto {desgasteLlantas = arreglarDosPrimerasLlantas (desgasteLlantas unAuto)}

lima :: Persona
lima unAuto = cambiarCubiertasDelanteras unAuto


-- PUNTO 4
-- dejo esto asi, esta casi perfecto, solamente me falta agregar una condicion mas para cuando solo
-- tenga una lista, despues lo veo. pero funciona jajajajaja
ordenToc :: [Auto] -> Bool

ordenToc [] = False
ordenToc (unAuto:segundoAuto:demasAutos) = tieneLlantaImpar unAuto && tieneLlantasPar segundoAuto && ordenToc demasAutos

tieneLlantasPar :: Auto -> Bool
tieneLlantasPar unAuto = even (round (sum (desgasteLlantas unAuto)))

tieneLlantaImpar :: Auto -> Bool
tieneLlantaImpar segundoAuto = odd (round (sum (desgasteLlantas segundoAuto)))

-- PUNTO 5
