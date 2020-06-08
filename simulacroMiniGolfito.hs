import Data.List -- para los metodos coleccionables que no vienen en la guia de lenguaje
--import Data.Text.IsInfixOf
import Text.Show.Functions -- para que las funciones dentro del data se vean <function>
import Data.Eq
import Data.Char

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int


between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b



-- ------------------------------------ - >1
type PaloDeGolf = Efecto
type Efecto = Habilidad -> Tiro -> Tiro
-- -- i
putter :: Efecto
putter unaHabilidad unTiro = (generaVelocidad 10 . generaPrecision (2* precision unTiro).generaAltura 0) unTiro

generaVelocidad :: Int -> Tiro -> Tiro
generaVelocidad unaVelocidad unTiro = unTiro {velocidad = unaVelocidad }

generaPrecision :: Int -> Tiro -> Tiro
generaPrecision unaPrecision unTiro = unTiro {precision = unaPrecision}

generaAltura :: Int -> Tiro -> Tiro
generaAltura unvalor unTiro = unTiro {altura = unvalor}

-- --ii
madera :: Efecto 
madera unaHabilidad unTiro =  (generaVelocidad 100. generaAltura 5 . generaPrecision ((*0.5) (precision unTiro))) unTiro

-- -- iii
hierros :: Int -> Efecto
hierros unValor unaHabilidad unTiro = (generaAltura (max (unValor -3) 0).generaPrecision ((precision unTiro)/unValor).generaVelocidad (unValor * (fuerzaJugador unaHabilidad))) unTiro

palos = [putter (Habilidad 20 30) , madera (Habilidad 30 10), hierros 10 (Habilidad 10 20)]

-- 2 
golpe unJugador unPalo = unPalo (habilidad unJugador)

--3 
tunelDeRampita unTiro | puedeSuperarObstaculo 90 0 0 unTiro = (generaVelocidad (2* velocidad unTiro).generaPrecision 100.generaAltura 0 ) unTiro
                      | otherwise = volverTiroObsoleto unTiro

laguna distanciaLaguna unTiro | puedeSuperarObstaculo 0 80 (altura unTiro) unTiro = generaAltura (altura unTiro / distanciaLaguna) unTiro
                              | otherwise = unTiro


hoyitoPeligroso unTiro | puedeSuperarObstaculo 95 (velocidad unTiro) 0 unTiro = volverTiroObsoleto unTiro
                       | otherwise = unTiro

puedeSuperarObstaculo unaPrecision _ _ unTiro = precision unTiro == unaPrecision
puedeSuperarObstaculo _ unaVelocidad unaAltura unTiro = velocidad unTiro == unaVelocidad && (estaEntre 1 5 (altura unTiro))
puedeSuperarObstaculo unaPrecision unaVelocidad _ unTiro = estaEntre 5 20 (unaVelocidad) && precision unTiro > unaPrecision


estaEntre unValor otroValor unaAltura = (unValor< unaAltura)  && (unaAltura< otroValor)

volverTiroObsoleto unTiro = (generaVelocidad 0 . generaPrecision 0.generaAltura 0 ) unTiro

-- 4
palosUtiles unaPersona unosPalos = map (golpe unaPersona) unosPalos

--b

-- cualesObstaculosSePuedeSuperar unTiro obstaculos  = 

--c
