import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
--import Data.Text.IsInfixOf
import Text.Show.Functions -- para que las funciones dentro del data se vean <function>
import Data.Eq
import Data.Char


data Genero = Genero {
    nombreGenero :: String,
    cantidadSeguidores :: Int,
    nivelDeDescontrolQueGenera :: Int
} deriving Show 
rock = Genero "rock" 311250 3
pop = Genero "pop" 53480 1
electronico = Genero "electronico" 10000 4
hipHop = Genero "hip-hop" 2000 1

data Banda = Banda{
    nombre :: String,
    seguidores :: Int,
    estilo :: [Genero]
} deriving Show

type Festival = [Banda]
cabalgataMusic = [aguaSinGas,djCutaneo]
heinekenRock = [cuadraditos,aguaSinGas]

aguaSinGas = Banda "aguaSinGas" 1000 [rock]
djCutaneo = Banda "djCutaneo" 1000 [electronico]
cuadraditos = Banda "cuadraditos" 1000 [pop]

--1
-- a 

nivelDeDescontrolTotal unGenero = cantidadSeguidores unGenero * nivelDeDescontrolQueGenera unGenero

-- b 
porcentajeRelevanciaDe unaBanda unGenero = fromIntegral (seguidores unaBanda)  / fromIntegral (sum (map cantidadSeguidores (estilo unaBanda)))

--c
nivelDeDescontrol unaBanda = sum (map nivelDeDescontrolTotal (estilo unaBanda)) * sum (map (porcentajeRelevanciaDe unaBanda) (estilo unaBanda))

--2
--a
mejorNivelDescontrol unGenero otroGenero 
    | nivelDeDescontrolTotal unGenero > nivelDeDescontrolTotal otroGenero = unGenero
    | otherwise = otroGenero 

--2b -1 
--obtenerMejorRendimiento [] = []
obtenerMejorRendimiento [genero1,genero2] = mejorNivelDescontrol genero1 genero2
obtenerMejorRendimiento (genero1:genero2:demasGeneros) = obtenerMejorRendimiento (mejorNivelDescontrol genero1 genero2 :demasGeneros)


--3
generoPrincipalDe unFestival = obtenerMejorRendimiento (estilo unFestival)

--4 

incorporarGeneroA unaBanda unGenero = (agregarGenero unGenero.sumarSeguidoresBanda ((+) (seguidores unaBanda))) unaBanda

agregarGenero unGenero unaBanda = unaBanda {estilo = unGenero : (estilo unaBanda)}
sumarSeguidoresBanda funcion unaBanda = unaBanda { seguidores = funcion (seguidores unaBanda )}
