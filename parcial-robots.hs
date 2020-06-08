
data Robot = UnRobot {
    nombre :: String,
    nivelDeNauseas :: Int,
    tickets :: Int
    }

type Premio = (String, Int)

nombrePremio :: Premio -> String
nombrePremio unPremio = fst unPremio

precioPremio :: Premio -> Int
precioPremio unPremio = snd unPremio

type Juego = (Robot -> Robot)

cambiarNausea :: (Int -> Int -> Int) -> Int -> Robot -> Robot
cambiarNausea operacion valor unRobot = unRobot {nivelDeNauseas = operacion (nivelDeNauseas unRobot) valor}

agregarAlNombre :: String -> Robot -> Robot
agregarAlNombre prefijo unRobot = unRobot {nombre = nombre unRobot ++ " " ++ prefijo}

aumentarTickets :: Int -> Robot -> Robot
aumentarTickets cantidad unRobot = unRobot {tickets = tickets unRobot + cantidad}

restarTickets :: Int -> Robot -> Robot
restarTickets cantidad unRobot = unRobot {tickets = max 0 (tickets unRobot - cantidad)}

nombreDelRobotEsMenorA :: Int -> Robot -> Bool
nombreDelRobotEsMenorA numero unRobot = (length.nombre) unRobot < numero 
---

zamba :: Int -> Juego
zamba vps unRobot = cambiarNausea (+) (5 * vps) unRobot

pumpIt :: Juego
pumpIt unRobot = (agregarAlNombre "bailarin".aumentarTickets 10) unRobot

daytona :: Int -> Juego
daytona 1 unRobot = aumentarTickets 20 unRobot
daytona 2 unRobot
    | nombreDelRobotEsMenorA 3 unRobot = aumentarTickets 30 unRobot
    | otherwise = unRobot
daytona 3 unRobot = cambiarNausea (+) 15 unRobot
daytona _ unRobot = unRobot

carreraDeCaballos :: Juego
carreraDeCaballos unRobot = restarTickets 30 unRobot

wallE :: Robot
wallE = UnRobot "Wall-E" 60 0

arturito :: Robot
arturito = UnRobot "Arturito" 10 35

-----punto B
type Prueba = [Robot] -> [Robot]

sigueFuncionando :: Robot -> Bool
sigueFuncionando unRobot = nivelDeNauseas unRobot < 80

pruebaBrasilera :: Prueba
pruebaBrasilera unosRobots = filter sigueFuncionando unosRobots

champions :: Prueba
champions unosRobots = (take 3.reverse.sortBy tickets) unosRobots

tourCompleto :: [Juego] -> Robot -> Robot
tourCompleto unosJuegos unRobot = (foldl1 (.) unosJuegos) unRobot

-----punto C

leAlcanzaPara :: Premio -> Robot -> Bool
leAlcanzaPara unPremio unRobot= precioPremio unPremio <= tickets unRobot

comprar :: Premio -> Robot -> Robot
comprar unPremio unRobot
    | leAlcanzaPara unPremio unRobot = restarTickets (precioPremio unPremio) unRobot
    | otherwise = unRobot

canje :: Robot -> [Premio] -> [Premio]
canje _ [] = []
canje unRobot [unPremio]
    | leAlcanzaPara unPremio unRobot = [unPremio]
    | otherwise = []
canje unRobot (unPremio:restoPremios)
    | leAlcanzaPara unPremio unRobot = [unPremio] ++ canje (comprar unPremio unRobot) restoPremios
    | otherwise = canje unRobot restoPremios

canjeMaximizado :: Robot -> [Premio] -> [Premio]
canjeMaximizado unRobot unosTickets = canje unRobot (sortBy id unosTickets)

-----parte D

premiarSiFunciona :: Robot -> Robot
premiarSiFunciona unRobot
    | sigueFuncionando unRobot = aumentarTickets 50 unRobot
    | otherwise = unRobot

montañaRusa :: Int -> Juego
montañaRusa asiento unRobot = (premiarSiFunciona.cambiarNausea (+) (asiento * 10)) unRobot 

-----parte E
infinitosRobots :: [Robot]
infinitosRobots = arturito : infinitosRobots

churrasquito :: Premio
churrasquito = ("Churrasquito", 35)

infinitosPremios :: [Premio]
infinitosPremios = churrasquito : infinitosPremios

--Si usamos por ejemplo (take 100.map (tourCompleto [zamba 1, zamba 1, zamba 1, zamba 1, zamba 1, daytona, daytona, daytona])) infinitosRobots, va a funcionar,
-- ya que take 100 evalua solo los primeros 100 elementos (el resto no, por Lazy Evaluation)

--Si usamos la función canje con infinitos premios, el programa no va a funcionar por cómo esta implementado: la función reverse necesita la 
--lista COMPLETA para poder evaluarla, y como en este caso es infinita, no funcionaría. (Si no estuviera reverse funcionaría perfectamente)