import Includes.Auth (confirmarUsuario)
import Includes.File (obtenerArchivo)
import Control.Monad (unless, when)

-- Muestra el menu operativo
menuOperativo :: IO ()
menuOperativo = do
    putStrLn "Bienvenido al sistema de administración de bicicletas"
    putStrLn "Escoja una de las siguientes opciones"
    putStrLn "1- Cargar y Mostrar parqueos"
    putStrLn "2- Mostrar bicicletas"
    putStrLn "3- Cargar usuarios"
    putStrLn "4- Estadísticas"
    putStrLn "5- Volver"
    putStr ">>"
    option <- getLine
    case option of  
        "1" -> putStrLn "Escoja una opcion valida"
        "2" -> putStrLn "Escoja una opcion valida"
        "3" -> putStrLn "Escoja una opcion valida"
        "4" -> putStrLn "Escoja una opcion valida"
        "5" -> return ()
        _ -> putStrLn "Escoja una opcion valida"
    Control.Monad.when (option /= "5") menuOperativo

-- Verifica el usuario entrante
auntentificacion :: IO ()
auntentificacion = do
    putStrLn "Usuario: "
    user <- getLine
    putStrLn "Contrasena: "
    contrase <- getLine
    if confirmarUsuario user contrase then
        menuOperativo
    else 
        auntentificacion

-- Muestra el menu principal
menuPrincipal :: IO ()
menuPrincipal = do
  putStrLn "Escoja una de las siguientes opciones"
  putStrLn "1- Menu operativo"
  putStrLn "2- Imprimir información"
  putStrLn "3- Salir"
  putStr ">>"
  option <- getLine
  case option of  
    "1" -> auntentificacion
    "2" -> menuPrincipal
    "3" -> return ()
    _ -> putStrLn "Escoja una opcion valida"
  Control.Monad.when (option /= "3") menuPrincipal

-- Inicia el programa y llama el menu principal
main :: IO ()
main = do
  putStrLn "Bienvenido al sistema de administración de bicicletas"
  menuPrincipal
