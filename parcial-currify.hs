data Cancion = Cancion {titulo :: String, genero :: String, duracion :: Int} deriving Eq
data Artista = Artista {nombre :: String, canciones :: [Cancion], efectoPreferido :: Efecto}
type Efecto = Cancion -> Cancion

-- modelos
cafeParaDos :: Cancion
cafeParaDos = Cancion {titulo = "Café para dos", genero = "rock melancólico", duracion = 146}

fuiHastaAhi :: Cancion
fuiHastaAhi = Cancion {titulo = "Fui hasta ahí", genero = "rock", duracion = 279}

rocketRaccoon :: Cancion
rocketRaccoon = Cancion {titulo = "Rocket Raccoon", genero = "rock", duracion = 180}

mientrasMiBateriaFesteja :: Cancion
mientrasMiBateriaFesteja = Cancion {titulo = "Mientras mi batería festeja", genero = "rock", duracion = 300}

tomateDeMadera :: Cancion
tomateDeMadera = Cancion {titulo = "Tomate de madera", genero = "rock", duracion = 256}

teAcordas :: Cancion
teAcordas = Cancion {titulo = "¿Te acordás?", genero = "reggaeton", duracion = 156}

unPibeComoVos :: Cancion
unPibeComoVos = Cancion {titulo = "Un pibe como vos", genero = "pop", duracion = 199}

daleMechaALaLluvia :: Cancion
daleMechaALaLluvia = Cancion {titulo = "Dale mecha a la lluvia", genero = "pop", duracion = 243}

losEscarabajos :: Artista
losEscarabajos = Artista {nombre = "Los escarabajos", canciones = [rocketRaccoon, mientrasMiBateriaFesteja, tomateDeMadera], efectoPreferido = acortar}

adela :: Artista
adela = Artista {nombre = "Adela", canciones = [teAcordas, unPibeComoVos, daleMechaALaLluvia], efectoPreferido = remixar}

elTigreJoaco :: Artista
elTigreJoaco = Artista {nombre = "El tigre Joaco", canciones = [], efectoPreferido = acustizar 6}

-- auxiliares
agregarAlTitulo :: String -> Cancion -> Cancion
agregarAlTitulo unaCadena unaCancion = unaCancion {titulo = (titulo unaCancion) ++ unaCadena}

cambiarGenero :: String -> Cancion -> Cancion
cambiarGenero unGenero unaCancion = unaCancion {genero = unGenero}

cambiarDuracionSegun :: (Int -> Int -> Int) -> Int -> Cancion -> Cancion
cambiarDuracionSegun operacion valorAOperar unaCancion = unaCancion {duracion = operacion (duracion unaCancion) valorAOperar}

esCancionCorta :: Cancion -> Bool
esCancionCorta unaCancion = ((<180).duracion) unaCancion

esCancionDelGenero :: String -> Cancion -> Bool
esCancionDelGenero unGenero unaCancion = genero unaCancion == unGenero

sonCancionesHomogeneas :: [Cancion] -> Bool
sonCancionesHomogeneas [] = True
sonCancionesHomogeneas [_] = True
sonCancionesHomogeneas [cancion1, cancion2] = genero cancion1 == genero cancion2
sonCancionesHomogeneas (cancion1:cancion2:otrasCanciones) = genero cancion1 == genero cancion2 && sonCancionesHomogeneas (cancion2:otrasCanciones)

cancionesDeTodos :: [Artista] -> [Cancion]
cancionesDeTodos unosArtistas = foldl (++) [] (map canciones unosArtistas)

efectosDeTodos :: [Artista] -> Efecto
efectosDeTodos unosArtistas = foldl (.) id (map efectoPreferido unosArtistas)

algunoEsDeGenero :: String -> String -> String -> Bool
algunoEsDeGenero generoAVerificar unGenero otroGenero = unGenero == generoAVerificar || otroGenero == generoAVerificar

mayorGeneroEntreDos :: String -> String -> String
mayorGeneroEntreDos unGenero otroGenero
    | algunoEsDeGenero "rock" unGenero otroGenero = "rock"
    | unGenero == "reggaeton" = otroGenero
    | otroGenero == "reggaeton" = unGenero
    | length unGenero > length otroGenero = unGenero
    | otherwise = otroGenero

generoSuperiorDelArtista :: Artista -> String
generoSuperiorDelArtista unArtista =  foldl1 mayorGeneroEntreDos ((map genero.canciones) unArtista)

-- efectos

acortar :: Efecto
acortar unaCancion = unaCancion {duracion = max 0 ((duracion unaCancion) - 60)}

remixar :: Efecto
remixar unaCancion = (cambiarDuracionSegun (*) 2.cambiarGenero "remixado".agregarAlTitulo "remix") unaCancion

acustizar :: Int -> Efecto
acustizar nuevaDuracion unaCancion
    | genero unaCancion == "acústico" = unaCancion
    | otherwise = unaCancion {genero = "acústico", duracion = nuevaDuracion}

metaEfecto :: [Efecto] -> Efecto
metaEfecto unosEfectos = foldl (.) id unosEfectos

-- punto B
vistazo :: Artista -> [Cancion]
vistazo unArtista = (take 3.filter esCancionCorta) (canciones unArtista)

playlist :: String -> [Artista] -> [Cancion]
playlist unGenero unosArtistas = filter (esCancionDelGenero unGenero) ((concat.map canciones) unosArtistas)

-- punto C
hacerseDJ :: Artista -> Artista
hacerseDJ unArtista = unArtista {canciones = map (efectoPreferido unArtista) (canciones unArtista)}

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo unArtista = (sonCancionesHomogeneas.canciones) unArtista

formarBanda :: String -> [Artista] -> Artista
formarBanda unNombre unosArtistas = Artista {nombre = unNombre, canciones = cancionesDeTodos unosArtistas, efectoPreferido = efectosDeTodos unosArtistas}

obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = Cancion {titulo = (concat.map titulo.canciones) unArtista, genero = generoSuperiorDelArtista unArtista ++ " progresivo", duracion = (sum.map duracion.canciones) unArtista}

-- punto D
--1) La artista no puede volverse DJ porque como tiene infinitas canciones, al intentar aplicar el efecto a cada una de sus canciones el programa
-- se va a colgar, haciendolo infinitamente.
--2) Si, podemos echar un vistazo a sus canciones debido a que, aunque sean infinitas, la funcion take 3 tiene Lazy Evaluation por lo que
-- no es necesario toda la lista para completar su cometido; sólo evalúa los primeros 3 elementos
--3) Tampoco va a poder crear una obra maestra progresiva. Análogamente con el punto 1), al tener infinitas canciones nunca va a terminar de
-- concatenar sus nombres (lo mismo con el género y la duración)