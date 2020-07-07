import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
--import Data.Text.IsInfixOf
import Text.Show.Functions -- para que las funciones dentro del data se vean <function>
import Data.Eq
import Data.Char


data Cancion = Cancion {
    titulo :: Titulo,
    genero :: Genero,
    duracion :: Duracion
} deriving Show

type Titulo = String
type Genero = String
type Duracion = Int

data Artista = Artista {
    nombre :: Nombre,
    cancion :: [Cancion],
    efecto :: Efecto
} deriving Show

type Nombre = String
type Efecto = Cancion -> Cancion

-- PARTE A
acortar :: Cancion -> Cancion
acortar unaCancion = crearCancion  (max ((duracion unaCancion) - 60) 0) unaCancion

crearCancion :: Int -> Cancion -> Cancion
crearCancion unaDuracion unaCancion = unaCancion {duracion = unaDuracion}

remixar :: Cancion -> Cancion
remixar unaCancion = (crearGenero "remixado".crearCancionConDobleDuracion (*2) . agregarFinalDelTitulo "remix") unaCancion

agregarFinalDelTitulo :: String -> Cancion -> Cancion
agregarFinalDelTitulo palabra unaCancion = unaCancion { titulo = (titulo unaCancion) ++ palabra}

crearGenero :: Genero -> Cancion -> Cancion
crearGenero unGenero unaCancion = unaCancion { genero = unGenero}

crearCancionConDobleDuracion funcion unaCancion = unaCancion { duracion = funcion (duracion unaCancion)}

acustizar :: Int -> Cancion -> Cancion
acustizar unaDuracion unaCancion  | not (esDeGenero "acustico" unaCancion) = (crearGenero "acustico" .crearCancion unaDuracion) unaCancion
                                  | otherwise = unaCancion
esDeGenero :: Genero -> Cancion -> Bool
esDeGenero unGenero unaCancion  =  genero unaCancion == unGenero


metaEfecto efectos unaCancion = foldl1 unaCancion efectos

----------------------------------------------------------- 
cafeParaDos = Cancion "cafe para dos" "rock medolico" 146
fuiHastaAhi = Cancion "fui hasta ahi" "rock" 279

losEscarabajos = Artista "los escarabajos" [rocketRaccoon,mientrasMiBateriaFesteja, tomateDemadera] acortar
adela = Artista "adela" [teAcordas , unPibeComoVos,daleMechaALaLluvia] remixar
elTigreJoaco = Artista "el tigre joaco" [] (acustizar 360)

rocketRaccoon = Cancion "rocket Raccoon" "rock medolico" 100

mientrasMiBateriaFesteja = Cancion "mientras Mi Bateria Festeja" "rock medolico" 120

tomateDemadera = Cancion "tomate Demadera" "rock medolico" 140

teAcordas = Cancion "te Acordas" "rock medolico" 160

unPibeComoVos = Cancion "un Pibe Como Vos" "rock medolico" 180

daleMechaALaLluvia = Cancion "dale Mecha A La Lluvia" "rock medolico" 200

-- PARTE B

vistazo unArtista = take 3 (filter esCorta (cancion unArtista))

esCorta unCancion = ((>=150).duracion) unCancion


-- playlist unGenero unosArtistas = filter (== unGenero) (map cancion (unosArtistas)) unosArtistas

-- PARTE C
hacerseDj unArtista = unArtista {cancion = map (efecto unArtista) (cancion unArtista)}

tieneGustoHomogeneo unArtista = all (perteneceMismoGenero) (cancion unArtista)

perteneceMismoGenero :: [Cancion] -> Bool
perteneceMismoGenero (unaCancion:otraCanciogenero unaCancion) ==  (genero otraCancion)n:demasCanciones) = (


formarBanda unNombre unosArtistas = crearArtista unNombre.(map )

crearArtista unNombre unArtista = unArtista {nombre = unNombre}

agregarCancionesDeLosArtistas unosArtistas = 