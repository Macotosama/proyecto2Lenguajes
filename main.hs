import Includes.Auth (confirmarUsuario)
import Includes.File (split,
    convertirString,
    arreglarString,
    obtenerLineas,
    leerArchivo,
    obtenerArchivo,
    filasString,
    listaString,
    escribirArchivo,
    agregarFila,
    imprimirFila,
    imprimirFilas,
    imprimirArchivo,
    buscarporParqueoAux,
    buscarPorParqueo)
import Control.Monad (unless, when)


-------------------------------------

mostraBicicletas :: String -> IO ()
mostraBicicletas bicicletas = do
  putStrLn "Bicicletas"
  putStrLn "Escriba el nombre del parqueo"
  putStrLn "O escriba '#' para todas las bicicletas del sistema"
  option <- getLine
  if "#" == option then imprimirArchivo bicicletas
  else buscarPorParqueo bicicletas option

-- Muestra el menu operativo
menuOperativo :: String -> String -> String -> IO ()
menuOperativo parqueos bicicletas usuarios = do
    putStrLn "Menu operativo"
    putStrLn "Escoja una de las siguientes opciones"
    putStrLn "1- Mostrar parqueos" 
    putStrLn "2- Mostrar bicicletas"
    putStrLn "3- Mostrar usuarios"
    putStrLn "4- Estadísticas"
    putStrLn "5- Volver"
    option <- getLine
    case option of  
        "1" -> imprimirArchivo parqueos
        "2" -> mostraBicicletas bicicletas
        "3" -> imprimirArchivo usuarios
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
