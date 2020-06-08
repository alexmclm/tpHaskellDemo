-----punto 1
data Pais = UnPais {
    ingresoPerCapita :: Float,
    activosSectorPublico :: Float,
    activosSectorPrivado :: Float,
    riqueza :: [String],
    deudaAlFMI :: Float
    } deriving Eq

namibia :: Pais
namibia = UnPais 4140 400000 650000 ["mineria", "ecoturismo"] 50

-----punto 2

type Receta = Pais -> Pais

reducirSectorPublico :: Float -> Pais -> Pais
reducirSectorPublico cantidad unPais = unPais {activosSectorPublico = activosSectorPublico unPais - cantidad}

cambiarIngresoPerCapita :: (Float -> Float -> Float) -> Float -> Pais -> Pais
cambiarIngresoPerCapita operacion proporcion unPais = unPais {ingresoPerCapita = operacion (ingresoPerCapita unPais) (ingresoPerCapita unPais * proporcion) }

calcularNuevoIngreso :: Float -> Pais -> Pais
calcularNuevoIngreso puestos unPais
    | puestos > 100 = cambiarIngresoPerCapita (-) 0.2 unPais
    | otherwise = cambiarIngresoPerCapita (-) 0.15 unPais

cambiarDeuda :: (Float -> Float -> Float) -> Float -> Pais -> Pais
cambiarDeuda operacion valor unPais = unPais {deudaAlFMI = operacion (deudaAlFMI unPais) valor}

quitarRiqueza :: String -> Pais -> Pais
quitarRiqueza unaRiqueza unPais = unPais {riqueza = filter (not.(== unaRiqueza)) (riqueza unPais)}

pbi :: Pais -> Float
pbi unPais = (ingresoPerCapita unPais) * (activosSectorPublico unPais + activosSectorPrivado unPais)

---

prestarMillonesDeDolares :: Float -> Receta
prestarMillonesDeDolares millones unPais = cambiarDeuda (+) (millones * 3) unPais

reducirPuestosDeTrabajo :: Float -> Receta
reducirPuestosDeTrabajo puestos unPais = (calcularNuevoIngreso puestos.reducirSectorPublico puestos) unPais

explotarRecurso :: String -> Receta
explotarRecurso recurso unPais = (quitarRiqueza recurso.cambiarDeuda (-) 2) unPais

establecerBlindaje :: Receta
establecerBlindaje unPais = (reducirPuestosDeTrabajo 500.prestarMillonesDeDolares (pbi unPais)) unPais

-----punto 3

prestarParaExplotar :: Receta
prestarParaExplotar unPais = (prestarMillonesDeDolares 200.explotarRecurso "Mineria") unPais

namibiaExplotado :: Pais
namibiaExplotado = prestarParaExplotar namibia

-----punto 4

cualesZafan :: [Pais] -> [Pais]
cualesZafan unosPaises = filter (elem "petroleo".riqueza) unosPaises

totalDeDeudas :: [Pais] -> Float
totalDeDeudas unosPaises = (sum.map deudaAlFMI) unosPaises

-----punto 5

tieneMenosPBI :: Pais -> Pais -> Bool
tieneMenosPBI unPais otroPais = pbi unPais < pbi otroPais

paisConMenosPBI :: Pais -> Pais -> Pais
paisConMenosPBI unPais otroPais
    | tieneMenosPBI unPais otroPais = unPais
    | otherwise = otroPais

estanRecetasOrdenadas :: Pais -> [Receta] -> Bool
estanRecetasOrdenadas _ [] = True
estanRecetasOrdenadas _ [_] = True
estanRecetasOrdenadas unPais [unaReceta, otraReceta] = tieneMenosPBI (unaReceta unPais) (otraReceta unPais)
estanRecetasOrdenadas unPais (unaReceta:otraReceta:restoRecetas) = tieneMenosPBI (unaReceta unPais) (otraReceta unPais)
                                                                    && estanRecetasOrdenadas unPais (otraReceta:restoRecetas)

-----punto 6

recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

neoArcadia :: Pais
neoArcadia = UnPais 15 1000 1001 recursosNaturalesInfinitos 45

--Si usamos la función cualesZafan con este país, el programa se va a quedar colgado porque va a intentar filtrar una lista infinita (aunque
-- sepamos que nunca va a cumplir la condición de tener petróleo). Esto es porque la función filter

--Si intentamos usar la función totalDeDeudas y uno de los paises es neoArcadia, va a funcionar debido a que la función sólo necesita evaluar
-- la deuda de los paises, cuyo campo no es infinito en ese caso. (Lazy Evaluation)