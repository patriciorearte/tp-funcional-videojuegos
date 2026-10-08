data Genero =
    Accion
    | Aventura
    | RPG
    | Deportes
    | Estrategia
    | Terror
    deriving (Show)

data Plataforma =
    PC
    | PlayStation
    | Xbox
    | Switch
    deriving (Show)

data Videojuego = Videojuego {
    titulo :: String,
    genero :: Genero,
    plataformas :: [Plataforma],
    precio :: Int,
    puntaje :: Int,
    disponible :: Bool
} deriving (Show)

godOfWar :: Videojuego
godOfWar = Videojuego {
    titulo = "God of War",
    genero = Accion,
    plataformas = [PC, PlayStation],
    precio = 50000,
    puntaje = 95,
    disponible = True
}

fifa :: Videojuego
fifa = Videojuego {
    titulo = "EA Sports FC 26",
    genero = Deportes,
    plataformas = [PC, PlayStation, Xbox],
    precio = 70000,
    puntaje = 82,
    disponible = True
}

residentEvil :: Videojuego
residentEvil = Videojuego {
    titulo = "Resident Evil 4",
    genero = Terror,
    plataformas = [PC, PlayStation, Xbox],
    precio = 45000,
    puntaje = 92,
    disponible = False
}

videojuegos :: [Videojuego]
videojuegos = [godOfWar, fifa, residentEvil]

-- Round-> redondea un decimal a entero , fromIntegral pasa de entero a decimal 
aplicarDescuento :: Float -> Videojuego -> Videojuego
aplicarDescuento porcentaje juego = juego { 
    precio = round (fromIntegral (precio juego) * (1 - porcentaje)) 
}

-- Cambiar disponibilidad usando Pattern Matching o Guardas
cambiarDisponibilidad :: Videojuego -> Videojuego
cambiarDisponibilidad juego = juego { disponible = not (disponible juego) }

-- FILTER: Obtener solo los videojuegos de un género específico
filtrarPorGenero :: Genero -> [Videojuego] -> [Videojuego]
filtrarPorGenero gBuscado = filter (\j -> genero j == gBuscado)

-- MAP + COMPOSICIÓN: Obtener solo las ofertas (juegos < 50000) pero con sus títulos en mayúsculas
-- Supongamos que usamos la función 'map' y composición de funciones (.)
titulosJuegosBaratos :: [Videojuego] -> [String]
titulosJuegosBaratos = map titulo . filter (\j -> precio j < 50000)

-- FOLDR: Obtener la suma total de precios de una lista de videojuegos
precioTotal :: [Videojuego] -> Int
precioTotal = foldr (\j acumulador -> precio j + acumulador) 0

-- FOLDR: Encontrar el videojuego con mayor puntaje (Juego del Año / GOTY)
juegoGOTY :: [Videojuego] -> Videojuego
juegoGOTY (primerJuego:resto) = foldr elMejor primerJuego resto
  where
    elMejor j1 j2 
      | puntaje j1 > puntaje j2 = j1
      | otherwise               = j2

-- RECURSIVIDAD: Saber si una plataforma está disponible en ALGUNO de los juegos de la lista
estaEnPlataforma :: Plataforma -> [Videojuego] -> Bool
estaEnPlataforma _ [] = False
estaEnPlataforma plat (x:xs) 
    | plat `elem` plataformas x = True
    | otherwise                = estaEnPlataforma plat xs