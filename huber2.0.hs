import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
--import Data.Text.IsInfixOf
import Text.Show.Functions -- para que las funciones dentro del data se vean <function>
import Data.Eq
import Data.Char

data Chofer = Chofer {
    nombre :: Nombre,
    km :: Km,
    viajes :: [Viaje],
    condicionViaje :: Condicion
} deriving Show 

type Nombre = String
type Km = Int
type Condicion = Viaje -> Bool
-- los ejemplos dice que tal chofer realizara un viaje donde el Cliente no viva en Olivos

data Viaje = Viaje {
    fecha :: Fecha,
    realizaViaje :: Cliente,
    costoViaje :: Int
} deriving Show

type Fecha = (Int,Int,Int)

data Cliente = Cliente {
    nombreCliente :: Nombre,
    lugarVivienda :: String
}deriving Show

-- 2
realizarCualquierViaje :: Viaje -> Bool
realizarCualquierViaje _ = True
-- sale de la chofer alejandra

realizarViaje :: Viaje -> Bool
realizarViaje  = (<200).costoViaje 

viajarConAquellosClientesCon :: Int -> Viaje -> Bool
viajarConAquellosClientesCon cantidaLetras unViaje  =  (length.nombreCliente.realizaViaje) unViaje  < cantidaLetras

realizaViajeMenosA :: String -> Viaje -> Bool
realizaViajeMenosA  zonaProhibida unViaje = zonaProhibida == (lugarVivienda.realizaViaje) unViaje

--3
lucas = Cliente "Lucas" "Victoria"
daniel = Chofer "Daniel" 23500 [Viaje (20,04,2017) lucas 150] (realizaViajeMenosA "Olivos")
alejandra = Chofer "Alejandra" 180000 [] realizarCualquierViaje

-- 4
puedeTomarCualquierViaje :: Viaje -> Chofer -> Bool
puedeTomarCualquierViaje unViaje unChofer  = condicionViaje unChofer unViaje
-- sale de que :t condicionViaje = Chofer -> Viaje

--5
liquidacionChofer :: Chofer -> Int
liquidacionChofer unChofer = sum (map costoViaje (viajes unChofer))

-- 6
realizarElViaje :: [Chofer] -> Viaje -> Chofer
realizarElViaje choferes unViaje = choferesConMenosViajes (filter (puedeTomarCualquierViaje unViaje) choferes)

choferesConMenosViajes :: [Chofer] -> Chofer
choferesConMenosViajes  [chofer] = chofer
choferesConMenosViajes (chofer:otroChofer:demasChoferes) | length (viajes chofer) < length (viajes otroChofer) = chofer
                                                         | otherwise = choferesConMenosViajes (otroChofer : demasChoferes)


-- 7

nitoInfy = Chofer "Nito Infy" 70000 infinitosViajes (viajarConAquellosClientesCon 3)


repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

elViajeInfinito  = Viaje (11,03,2017) lucas 50

infinitosViajes = repetirViaje elViajeInfinito