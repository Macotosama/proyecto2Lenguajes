import Includes.Auth (confirmarUsuario)
import Includes.File (split,
  convertirString,
  arreglarString,
  obtenerLineas,
  leerArchivo,
  obtenerArchivo,
  filasString,
  listaString,
  escribirArchivo)
import Control.Monad (unless, when)
import Text.XHtml (dir)
import System.Win32 (COORD(y))

nuevoParqueo :: String -> IO ()
nuevoParqueo parqueos = do
  parqueos <- obtenerArchivo parqueos
  putStrLn "Ingrese el nombre del parqueo"
  nombre <- getLine
  putStrLn "Ingrese la direccion del parqueo"
  dir <- getLine
  putStrLn "Ingrse las provincias del parqueo"
  pro <- getLine
  putStrLn "Ingrese las coordenadas x del parqueo"
  equis <- getLine
  putStrLn "Ingrese las coordenadas y del parqueo"
  ye <- getLine
  escribirArchivo "./datos/parqueos.csv" (take 1 parqueos ++ [[nombre, dir, pro, equis, ye]])

menuParqueos :: String -> IO ()
menuParqueos parqueos = do
  putStrLn "Parqueos"
  putStrLn "1- Cargar parqueo"
  putStrLn "2- Mostrar parqueos"
  putStrLn "3- Volver"
  option <- getLine
  case option of
    "1" -> nuevoParqueo parqueos
    "2" -> putStrLn "En espera"
    "3" -> return ()
    _ -> putStrLn "Escoja una opcion valida"
  Control.Monad.when (option /= "3") (menuParqueos parqueos)

-- Muestra el menu operativo
menuOperativo :: String -> String -> String -> IO ()
menuOperativo parqueos bicicletas usuarios = do
    putStrLn "Menu operativo"
    putStrLn "Escoja una de las siguientes opciones"
    putStrLn "1- Cargar y Mostrar parqueos"
    putStrLn "2- Mostrar bicicletas"
    putStrLn "3- Cargar usuarios"
    putStrLn "4- Estadísticas"
    putStrLn "5- Volver"
    option <- getLine
    case option of  
        "1" -> menuParqueos parqueos
        "2" -> putStrLn "Escoja una opcion valida"
        "3" -> putStrLn "Escoja una opcion valida"
        "4" -> putStrLn "Escoja una opcion valida"
        "5" -> return ()
        _ -> putStrLn "Escoja una opcion valida"
    Control.Monad.when (option /= "5") (menuOperativo parqueos bicicletas usuarios)

-- Verifica el usuario entrante
auntentificacion :: String -> String -> String -> IO ()
auntentificacion parqueos bicicletas usuarios = do
    putStrLn "Usuario: "
    user <- getLine
    putStrLn "Contrasena: "
    contrase <- getLine
    if confirmarUsuario user contrase then
        menuOperativo parqueos bicicletas usuarios
    else 
        auntentificacion parqueos bicicletas usuarios

-- Muestra el menu principal
menuPrincipal :: String -> String -> String -> IO ()
menuPrincipal parqueos bicicletas usuarios = do
  putStrLn "Escoja una de las siguientes opciones"
  putStrLn "1- Menu operativo"
  putStrLn "2- Imprimir información"
  putStrLn "3- Salir"
  option <- getLine
  case option of  
    "1" -> auntentificacion parqueos bicicletas usuarios
    "2" -> menuPrincipal parqueos bicicletas usuarios
    "3" -> return ()
    _ -> putStrLn "Escoja una opcion valida"
  Control.Monad.when (option /= "3") (menuPrincipal  parqueos bicicletas usuarios)

-- Inicia el programa y llama el menu principal
main :: IO ()
main = do
  putStrLn "Bienvenido al sistema de administración de bicicletas"
  putStrLn "Ingrese la ruta del archivo de parqueos"
  parqueos <- getLine
  putStrLn "Ingrese la ruta del archivo de bicicletas"
  bicicletas <- getLine
  putStrLn "Ingrese la ruta del archivo de usuario"
  usuarios <- getLine
  menuPrincipal parqueos bicicletas usuarios
