-----PRIMERA PARTE
-----punto 1
data Personaje = UnPersonaje {
    nombre :: String,
    edad :: Int,
    energia :: Int,
    planeta :: String,
    habilidades :: [Habilidad]
    }

data Guantelete = UnGuantelete {
    material :: String,
    gemas :: [Gema]
}

type Habilidad = String
type Gema = (Personaje -> Personaje)
type Universo = [Personaje]

poseeTodasLasGemas :: Guantelete -> Bool
poseeTodasLasGemas unGuantelete = ((==6).length.gemas) unGuantelete

esAptoParaChasquear :: Guantelete -> Bool
esAptoParaChasquear unGuantelete = poseeTodasLasGemas unGuantelete && ((== "uru").material) unGuantelete

reducirALaMitad :: Universo -> Universo
reducirALaMitad unUniverso = take ((div 2.length) unUniverso) unUniverso

chasquearUniverso :: Universo -> Guantelete -> Universo
chasquearUniverso unUniverso unGuantelete
    | esAptoParaChasquear unGuantelete = reducirALaMitad unUniverso
    | otherwise = unUniverso

-----punto 2

tieneMasDeUnaHabilidad :: Personaje -> Bool
tieneMasDeUnaHabilidad unPersonaje = ((>1).length.habilidades) unPersonaje
--

esAptoParaPendex :: Universo -> Bool
esAptoParaPendex unUniverso = any ((<45).edad) unUniverso

energiaTotal :: Universo -> Int
energiaTotal unUniverso = (sum.map energia.filter tieneMasDeUnaHabilidad) unUniverso

-----SEGUNDA PARTE
-----punto 3

modificarEnergia :: (Int -> Int -> Int) -> Int -> Personaje -> Personaje
modificarEnergia operacion operando unPersonaje = unPersonaje {energia = operacion (energia unPersonaje) operando}

eliminarHabilidad :: Habilidad -> Personaje -> Personaje
eliminarHabilidad unaHabilidad unPersonaje = unPersonaje {habilidades = filter (not.(== unaHabilidad)) (habilidades unPersonaje)} 

transportarAlPlaneta :: String -> Personaje -> Personaje
transportarAlPlaneta nuevoPlaneta unPersonaje = unPersonaje {planeta = nuevoPlaneta}

tienePocasHabilidades :: Personaje -> Bool
tienePocasHabilidades unPersonaje = ((<=2).length.habilidades) unPersonaje

quitarHabilidades :: Personaje -> Personaje
quitarHabilidades unPersonaje
    | tienePocasHabilidades unPersonaje = unPersonaje {habilidades = []}
    | otherwise = unPersonaje

acortarEdad :: Personaje -> Personaje
acortarEdad unPersonaje = unPersonaje {edad = max 18 (div 2 (edad unPersonaje))}
--

laMente :: Int -> Gema
laMente energiaAQuitar unPersonaje = modificarEnergia (-) energiaAQuitar unPersonaje

elAlma :: Habilidad -> Gema
elAlma unaHabilidad unPersonaje = (modificarEnergia (-) 10.eliminarHabilidad unaHabilidad) unPersonaje

elEspacio :: String -> Gema
elEspacio unPlaneta unPersonaje = (modificarEnergia (-) 20.transportarAlPlaneta unPlaneta) unPersonaje

elPoder :: Gema
elPoder unPersonaje = (modificarEnergia (-) (energia unPersonaje).quitarHabilidades) unPersonaje

elTiempo :: Gema
elTiempo unPersonaje = (modificarEnergia (-) 50.acortarEdad) unPersonaje

laGemaLoca :: Gema -> Gema
laGemaLoca unaGema unPersonaje = (unaGema.unaGema) unPersonaje

-----punto 4

guanteleteMalisimo :: Guantelete
guanteleteMalisimo = (UnGuantelete "goma" [elTiempo, elAlma "usar Mjolnir", laGemaLoca (elAlma "programación en Haskell")])

-----punto 5

utilizar :: [Gema] -> Personaje -> Personaje
utilizar unasGemas unEnemigo = (foldl1 (.) unasGemas) unEnemigo

-----punto 6

mejorGema :: Gema -> Gema -> Personaje -> Gema
mejorGema unaGema otraGema unPersonaje
    | (energia.unaGema) unPersonaje < (energia.otraGema) unPersonaje = unaGema
    | otherwise = otraGema
--

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa (UnGuantelete _ []) _ = id
gemaMasPoderosa (UnGuantelete _ [unaGema]) _ = unaGema
gemaMasPoderosa (UnGuantelete _ [unaGema, otraGema]) unPersonaje = mejorGema unaGema otraGema unPersonaje
gemaMasPoderosa (UnGuantelete _ (unaGema:otraGema:restoGemas)) unPersonaje = gemaMasPoderosa (UnGuantelete "" ((mejorGema unaGema otraGema unPersonaje):restoGemas)) unPersonaje

-----punto 7

--a) guanteleteDeLocos no puede ser parámetro de la función gemaMasPoderosa, ya que ésta termina cuando ya evaluó todas las gemas, y si hay
-- gemas infinitas, la función se queda colgada para siempre.
--b) A diferencia de lo anteriormente dicho, usoLasTresPrimeras sí puede usar el guanteleteDeLocos, porque como se hace uso del concepto de
-- Lazy Evaluation, la función take 3 toma las primeras 3 gemas del guantelete infinito, sin evaluar las otras infinitas.