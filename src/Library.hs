module Library where
import PdePreludat

-- PARTE A --

data Ninja = UnNinja {
    nombre :: String,
    herramientas :: [Herramienta],
    jutsus :: [String],
    rango :: Rango
} deriving(Show, Eq)

eze :: Ninja
eze = UnNinja "Ezequiel" [UnaHerramienta "arma1" 2, UnaHerramienta "arma2" 4] ["holi"] 1

-- El rango es un número que comienza en 0 y no puede ser negativo.
type Rango = Number 

-- Las herramientas ninjas son de mucha ayuda para realizar misiones. De cada una conocemos el nombre y la cantidad disponible.
-- Algunos ejemplos son: bombas de humo, kunais, shurikens y sellos explosivos. 

data Herramienta = UnaHerramienta {
    nombreHerramienta :: String,
    cantDisponible :: Number
} deriving(Show, Eq)

type Herramienta' = (String, Number)

-- Para poder utilizarlas se pide modelar:

-- a. obtenerHerramienta: cada ninja debe poder obtener una cantidad específica de una herramienta en particular 
-- teniendo en cuenta que:
-- i.  si la suma de todas sus herramientas más la cantidad a obtener es menor o igual a 100, puede hacerlo sin problemas;
-- ii. en caso contrario, obtiene la cantidad que pueda sin excederse de 10 herramientas.

obtenerHerramienta :: Number -> String -> Ninja -> Ninja
obtenerHerramienta cantidad herramienta ninja
    | cantidadDeHerramientas ninja + cantidad <= 100 = agregarHerramienta cantidad herramienta ninja
    | otherwise                                      = agregarHerramienta (max 10 cantidad) herramienta ninja  

cantidadDeHerramientas :: Ninja -> Number
cantidadDeHerramientas = sum . map cantDisponible . herramientas

agregarHerramienta :: Number -> String -> Ninja -> Ninja
agregarHerramienta cantidad nuevaHerramienta ninja = ninja {herramientas = UnaHerramienta nuevaHerramienta cantidad : herramientas ninja}

--agregarHerramienta' :: Number -> String -> Ninja -> Ninja
--agregarHerramienta' cantidad nuevaHerramienta ninja = ninja {herramientas = (nuevaHerramienta,cantidad) : herramientas ninja}

-- b. usarHerramienta: cuando un ninja usa alguna de sus herramientas no mide cuántas utiliza, por lo que se queda 
-- sin ella y no debe figurar más entre sus pertenencias.

--usarHerramienta :: String -> Ninja -> Ninja
--usarHerramienta = eliminarHerramienta

--eliminarHerramienta :: String -> Ninja -> Ninja
--eliminarHerramienta herramienta ninja = ninja {herramientas = filter (/= herramienta . map nombreHerramienta) (herramientas ninja)}
