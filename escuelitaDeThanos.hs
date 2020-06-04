import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
--import Data.Text.IsInfixOf
import Text.Show.Functions -- para que las funciones dentro del data se vean <function>
import Data.Eq
import Data.Char

data Guantelete = Guantelete {
    material :: Material,
    gema :: [Gema]
}deriving Show

type Material = String
type Gema = Personaje -> Personaje

data Personaje = Personaje {
    edad :: Int,
    energia :: Energia,
    nombre :: Nombre,
    habilidad :: [Habilidad],
    planetaHogar :: PlanetaHogar
}deriving Show

type Nombre = String
type Energia = Int
type PlanetaHogar = String
type Habilidad = String

type Universo = [Personaje]

chasquear :: Guantelete -> Universo -> Universo
chasquear unGuantelete unUniverso | tieneGuanteCompleto unGuantelete = take (mitadUniverso unUniverso) unUniverso
                                  | otherwise = unUniverso

tieneGuanteCompleto :: Guantelete -> Bool
tieneGuanteCompleto unGuantelete = length (gema unGuantelete) == 6 || ((== "uru").material) unGuantelete

mitadUniverso :: Universo -> Int
mitadUniverso universo = fromIntegral (length universo) / genericLength 2


-- 2
esAptoPendex :: Universo -> Bool
esAptoPendex unUniverso = any (esMenorA 45) unUniverso

esMenorA :: Int -> Personaje -> Bool
esMenorA unaEdad unPersonajeDelUniverso = (>unaEdad) (edad unPersonajeDelUniverso)

energiaTotalUniverso :: Universo -> Int
energiaTotalUniverso unUniverso =  sum (map energia (filter tieneAlMenos1Habilidad unUniverso))
-- filtro los personajes que tienen solo una habilidad, pero en realidad quiero la energia de estos 

tieneAlMenos1Habilidad :: Personaje -> Bool
tieneAlMenos1Habilidad personajeDelUniverso = ((>1).length.habilidad) personajeDelUniverso

-- segunda parte
laMente unValor unPersonaje = modificarEnergia (+ unValor) unPersonaje

modificarEnergia funcion  personaje = personaje {energia = funcion (energia personaje)}
--

elAlma habilidadAsacar unPersonaje = (modificarEnergia (+10). eliminarHabilidad habilidadAsacar) unPersonaje

eliminarHabilidad unaHabilidad unPersonaje = unPersonaje {habilidad = filter (not.(== unaHabilidad)) (habilidad unPersonaje) } 

esLaHablidadASacar habilidadAsacar unPersonaje = elem  habilidadAsacar (habilidad unPersonaje)
--

elEspacio unPersonaje = modificarEnergia (+(-20)) unPersonaje
--

elPoder unPersonaje = (modificarEnergia (+(- energia unPersonaje)). habilidadesMayorA2) unPersonaje

habilidadesMayorA2 unPersonaje | (length habilidad) > 2 = quitarHabilidades unPersonaje
                               | otherwise = unPersonaje

quitarHabilidades unPersonaje = unPersonaje { habilidad = []}

--
elTiempo unPersonaje = modificarEnergia (+(- 50)).reducirAmitadEdadaOponente (edad unPersonaje /2) 

reducirAmitadEdadaOponente unaEdad unPersonaje = unPersonaje {edad = max unaEdad 18}

--
laGemaLoca gema = gema.gema

-- punto 3
--gemasInfinito = [laMente , elAlma, elEspacio, elPoder, elTiempo, laGemaLoca]


--gemaDelInfinito :: [Gema]
--gemaDelInfinito = laMente 10 : elAlma "auto": elEspacio:elPoder:elTiempo:laGemaLoca

-- punto 4
justin = Personaje 20 10 "justin" ["volar"] "felicidonia"
guanteleteCosmico = Guantelete "goma" [elTiempo justin , elAlma "usar mjolnir" justin]

-- laGemaLoca "programacion en haskell"

-- PUNTO 5
utilizar unasGemas unPersonaje = foldl1 unPersonaje unasGemas

-- PUNTO 6
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa unGuantelete unPersonaje = gemaMasPoderosaDe (gema unGuantelete) unPersonaje

gemaMasPoderosaDe :: [Gema] -> Personaje -> Gema
gemaMasPoderosaDe [gema] _ = gema
gemaMasPoderosaDe (gema1:gema2:demasGemas) unPersonaje | (energia.gema1) unPersonaje > (energia.gema2) unPersonaje = gemaMasPoderosaDe (gema1:demasGemas) unPersonaje
                                                     | otherwise = gemaMasPoderosaDe (gema2:demasGemas) unPersonaje


-- PUNTO 7
-- a 
    -- de ejecutar , lo ejecuta, pero no devolvera la gema mas poderosa debido a que la lista de tiene el guanteleteCosmico
    -- es infinita
-- b
    -- paso jajajaj