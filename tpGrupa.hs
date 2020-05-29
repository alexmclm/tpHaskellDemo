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
ford = Auto "AT001LN" [0.3, 0.1, 0.1, 0.2] 10000 100 (2,1,2015)

audi :: Auto
audi = Auto "DJV214" [0.5, 0.5, 0.5, 0.6] 2948 80 (3,10,2016)

chevrolet :: Auto
chevrolet = Auto "DJV215" [0.1, 0.5, 0.2, 0.2] 5893 63 (16,8,2019)

ferrari :: Auto
ferrari = Auto "DFH029" [0.5, 0.4, 0.1, 0.6] 9382 110 (13,5,2014)

autos :: [Auto]
autos = [ford, audi, chevrolet, ferrari]

autosDesordenados :: [Auto]
autosDesordenados = [audi, ford, ferrari, chevrolet]

mecanicos :: [Persona]
mecanicos = [alfa, bravo, zulu]

navidad :: Fecha
navidad = (25,12,2020)


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

ordenToc :: [Auto] -> Bool
ordenToc [] = True
ordenToc [unAuto] = tieneLlantasImpar unAuto
ordenToc (unAuto:segundoAuto:demasAutos) = tieneLlantasImpar unAuto && tieneLlantasPar segundoAuto && ordenToc demasAutos

tieneLlantasPar :: Auto -> Bool
tieneLlantasPar unAuto = (even.round.(*10).sum.desgasteLlantas) unAuto

tieneLlantasImpar :: Auto -> Bool
tieneLlantasImpar unAuto = (not.tieneLlantasPar) unAuto

-- PUNTO 5

cambiarFecha :: Fecha -> Auto -> Auto
cambiarFecha unaFecha unAuto = unAuto {ultimoArreglo = unaFecha}

ordenReparacion :: Fecha -> [Persona] -> Auto -> Auto
ordenReparacion unaFecha [] unAuto = cambiarFecha unaFecha unAuto
ordenReparacion unaFecha (mecanico:otrosMecanicos) unAuto  = ordenReparacion unaFecha otrosMecanicos (mecanico unAuto)  

-- PUNTO 6
--PARTE 1 (Facu)
loDejaEnCondiciones :: Auto -> Persona -> Bool
loDejaEnCondiciones unAuto unaPersona = (not.esPeligroso.unaPersona) unAuto

tecnicosSuficientes :: [Persona] -> Auto -> [Persona]
tecnicosSuficientes unosTecnicos unAuto = filter (loDejaEnCondiciones unAuto) unosTecnicos

--PARTE 2 (Alex)
costoReparacionA :: [Auto]-> Int
costoReparacionA unosAutos = (sum.map costoReparacion.autosEnRevision) unosAutos

autosEnRevision :: [Auto]-> [Auto]
autosEnRevision unosAutos = filter necesitaRevision unosAutos

--PUNTO 7
--PARTE 1 (Facu)

--RESPUESTA: Si, se puede obtener el primer tecnico que dejaria el auto en condiciones obteniendo el primer elemento de la lista generada
-- por tecnicosSuficientes. Esto se puede hacer debido a que la funcion head tiene Lazy Evaluation; una vez que ya tiene el elemento que
-- busca, no evalua el resto.
tecnicosInfinitos :: [Persona]
tecnicosInfinitos = alfa:bravo:charly:tango:lima:zulu:tecnicosInfinitos

primerTecnicoSuficiente :: [Persona] -> Auto -> Persona
primerTecnicoSuficiente unosTecnicos unAuto = (head.tecnicosSuficientes unosTecnicos) unAuto

--PARTE 2 (Alex)