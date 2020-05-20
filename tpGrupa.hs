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

ford = Auto "DJA324234" [0,2] 10000 100 (2,1,512)

-- PUNTO 1

-- costo reparacion de auto

costoReparacion unAuto | cantidadDigitosPatentes 7 unAuto = 12500
--                      | patenteConLetras unAuto = calculoParaPatentesPeculiar unAuto
                       | otherwise = 20000

cantidadDigitosPatentes unDigito unAuto = length (patente unAuto) == 7

-- recorda que elem toma si o si una lista, explicitamente [] asi , que considere string como lista de caracteres, no es valido
--patenteConLetras unAuto = elem "DJ" (patente unAuto) || elem "NB" (patente unAuto)

--calculoParaPatentesPeculiar unAuto | last (patente unAuto) == '4' = 3000* length (patente unAuto)
--                                   | otherwise = 20000

-- Punto 2
esPeligroso unAuto = head (desgasteLlantas unAuto) < 0.5

anioRevision (_,_,anio) = anio

necesitaRevision unAuto = (anioRevision unAuto) < 2015

-- punto 3

alfa unAuto | (rpm unAuto) > 2000 = regularAuto 2000 unAuto
            | otherwise = id unAuto

regularAuto unaCantidad unAuto = unAuto {rpm = unaCantidad}

bravo unAuto = unAuto {desgasteLlantas = [0]}

charly unAuto = (alfa.bravo) unAuto

tango unAuto = id unAuto

-- zulu 
