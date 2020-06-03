data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)

data Gema = UnaGema {
    nombre :: String,
    fuerza :: Int,
    personalidad :: Personalidad
}

type Personalidad = Situacion -> Situacion
type Situacion = [Aspecto]

peleaDeVergas :: Situacion
peleaDeVergas = [UnAspecto "incertidumbre" 3.6, UnAspecto "tensión" 6, UnAspecto "peligro" 4.5]

incertidumbre :: Aspecto
incertidumbre = (UnAspecto "incertidumbre" 0)

peligro :: Aspecto
peligro = (UnAspecto "peligro" 0)

tension :: Aspecto
tension = (UnAspecto "tensión" 0)

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> Situacion -> Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo :: String -> Situacion -> Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto :: Aspecto ->Situacion -> Situacion
reemplazarAspecto aspectoBuscado situacion = aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)

-----1)

modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto operacion unAspecto =  unAspecto {grado = operacion (grado unAspecto)}

mejorSituacion :: Situacion -> Situacion -> Bool
mejorSituacion [] [] = False
mejorSituacion [aspecto1] [aspecto2] = mejorAspecto aspecto1 aspecto2
mejorSituacion (aspecto1:resto1) peor = mejorAspecto aspecto1 (buscarAspecto aspecto1 peor) && mejorSituacion resto1 peor
mejorSituacion _ _ = False

modificarSituacion :: (Float -> Float) -> Aspecto -> Situacion -> Situacion
modificarSituacion operacion aspectoBuscado unaSituacion = (modificarAspecto operacion (buscarAspecto aspectoBuscado unaSituacion)):(reemplazarAspecto aspectoBuscado unaSituacion)

-----2)

vidente :: Personalidad
vidente unaSituacion = (modificarSituacion (subtract 10) tension.modificarSituacion (/2) incertidumbre) unaSituacion

relajada :: Float -> Personalidad
relajada unidadRelajo unaSituacion = (modificarSituacion (+ unidadRelajo) peligro.modificarSituacion (subtract 30) tension) unaSituacion

amatista :: Gema
amatista = UnaGema "Amatista" 400 (relajada 10)

perla :: Gema
perla = UnaGema "Perla" 320 vidente

-----3)

leGana :: Situacion -> Gema -> Gema -> Bool
leGana unaSituacion unaGema otraGema = fuerza unaGema >= fuerza otraGema && mejorSituacion ((personalidad unaGema) unaSituacion) ((personalidad otraGema) unaSituacion)

-----4)

nombreDeFusion :: Gema -> Gema -> String
nombreDeFusion unaGema otraGema
    | nombre unaGema == nombre otraGema = nombre unaGema
    | otherwise = nombre unaGema ++ nombre otraGema 

bajarEn10 :: Aspecto -> Aspecto
bajarEn10 unAspecto = unAspecto {grado = (grado unAspecto) - 10}

disminuirTodosAspectos :: Situacion -> Situacion
disminuirTodosAspectos unaSituacion = map bajarEn10 unaSituacion

personalidadFusion :: Gema -> Gema -> Personalidad
personalidadFusion unaGema otraGema = personalidad otraGema.personalidad unaGema

sonCompatibles :: Situacion -> Gema -> Gema -> Bool
sonCompatibles unaSituacion unaGema otraGema = mejorSituacion ((personalidadFusion unaGema otraGema) unaSituacion) ((personalidad unaGema) unaSituacion)
    && mejorSituacion ((personalidadFusion unaGema otraGema) unaSituacion) ((personalidad otraGema) unaSituacion)

gemaDominante :: Situacion -> Gema -> Gema -> Gema
gemaDominante unaSituacion unaGema otraGema
    | leGana unaSituacion unaGema otraGema = unaGema
    | otherwise = otraGema

calculoDeFuerzaFusion :: Situacion -> Gema -> Gema -> Int
calculoDeFuerzaFusion unaSituacion unaGema otraGema
    | sonCompatibles unaSituacion unaGema otraGema = ((*10).sum.map fuerza) [unaGema, otraGema]
    | otherwise = ((*7).fuerza.gemaDominante unaSituacion unaGema) otraGema

fusion :: Situacion -> Gema -> Gema -> Gema
fusion unaSituacion unaGema otraGema = UnaGema {
    nombre = nombreDeFusion unaGema otraGema,
    fuerza = calculoDeFuerzaFusion unaSituacion unaGema otraGema,
    personalidad = personalidadFusion unaGema otraGema.disminuirTodosAspectos
    }

-----5)

fusionGrupal :: Situacion -> [Gema] -> Gema
fusionGrupal unaSituacion unasGemas = foldl1 (fusion unaSituacion) unasGemas

-----6)


foo :: (Eq c) => a -> (a -> c) -> (b -> [c]) -> b -> Bool
foo x y z = any (== y x).z