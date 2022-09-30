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

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time (UniversalTime, getCurrentTime, Day)
import Text.Printf (printf)
import Text.Read (readMaybe)



-------------------------------------Tim

--Funcion encargada de retornar un alquiler
regresarAlquilerActivo::[[[Char]]]->String->[[[Char]]]
regresarAlquilerActivo nLista idAlquiler = do 
  if nLista /= [] then do
    if head(head nLista) == idAlquiler then do
      if last(head nLista) == "activo" then do
        return (head nLista)
      else
         return []
    else do
      regresarAlquilerActivo (tail nLista) idAlquiler
  else do   
    --putStrLn "\t\tEl Codigo de alquiler no existe...XoX"
    return []

--Funcion encargada de verificar si un alquiler se encuentra en estado activo
verificarAlquilerActivo::[[[Char]]]->String->Int
verificarAlquilerActivo nLista idAlquiler = do 

  if null nLista then do
    0
  else    
    --putStrLn "\t\tEl Codigo de alquiler no existe...XoX"
    if head(head nLista) == idAlquiler then do
      
      if last(head nLista) == "activo" then do
        1
      else
        0
    else do
      verificarAlquilerActivo (tail nLista) idAlquiler

--Funcion encargada de retornar una opcion de total de kilometros dependiendo de la bicicleta
elegirTarifaTienda::String->String->Int
elegirTarifaTienda nTipoDeBici nCantidadDeKilometros = do 
  let totalKilometros = read (nCantidadDeKilometros) :: Int
  if nTipoDeBici == "TR" then do
    totalKilometros * 1000
  else do 
    totalKilometros * 500


facturar::IO ()
facturar = do
  putStrLn "\t\t<<<<Opciones de facturacion>>>>"
  archivoFacturas <- obtenerLineas "./datos/facturas.csv" -- informacion de facturas
  alquileresArchivo <- obtenerLineas "./datos/alquiler.csv" --informacion de alquileres
  infoNegocio <- obtenerLineas "./datos/infonegocio.csv" --informacion precargada
  putStrLn "\t\tIngrese el identificador del alquiler"
  nIdAlquilerPasword <- getLine

  putStrLn "\t\tIngrese el identificador o codigo de la bicicleta"
  bicicleta <- getLine

  
  putStrLn "\t\tIngrese el punto de llegada"
  puntoLlegada <- getLine
  
  --if (verificarAlquilerActivo [alquileresArchivo] nIdAlquilerPasword) == 1 then do

  cambiarEstadoBici bicicleta puntoLlegada
  putStrLn "\t\tIngrese la cantidad de kilometros que recorrio"
  nCantidadDeKilometros <-getLine

  putStrLn "\t\tIngrese el tipo de bicicleta que utilizo (TR o EL)"
  nTipoDeBici <- getLine



  escribirArchivo "./datos/facturas.csv" ([archivoFacturas] ++ [infoNegocio]++[[nIdAlquilerPasword++nTipoDeBici]]++(regresarAlquilerActivo [alquileresArchivo] nIdAlquilerPasword)++[[show(elegirTarifaTienda nTipoDeBici nCantidadDeKilometros)]]) --falta agregar la informacion extraida del alquiler


  
-------------------------------------Tim
-- funcion encargada de poner una bicicleta en modo alquiler
cambiarEstadoBici :: String->String->IO()
cambiarEstadoBici identificador estado = do --el didentificador es el id de la bici que se desea alquilar y el estado es el estado en el cual se encuentra, esta puede ser 1 para activo o 0 para inactivo 
  archivo <- obtenerArchivo "./datos/bicicletas.csv"
  cambiarEstadoBiciAux archivo identificador estado 0



--Funcion encargada auxiliar la funcion de poner bici en modo alquiler
cambiarEstadoBiciAux :: [[[Char]]]->String->String->Int-> IO()
cambiarEstadoBiciAux nLista identificador estado contador = do
--Se vuelve a llamar a la funcion para que se actualicen los valores que se fueron perdiendo en el proceso de recorte con (tail nLista)
  
  
  if null nLista then do
    return ()
  else do
    putStrLn "pasa por aqui x2"
    if head(head nLista) == identificador 
      then do
        nLista2 <- obtenerArchivo "./datos/bicicletas.csv"
        let mitad = take 2 (head nLista) ++ [estado]
        let primero = (take (contador) (nLista2))
        let segundo = (drop (contador+1) (nLista2))
        putStrLn "pasa por aqui"
        escribirArchivo "./datos/bicicletas.csv" (primero++[mitad]++segundo)
        --escribirArchivo "./data/bicicletas.csv" (take (contador-1) (head nLista)) ++ [take 2 head(head nLista) ++ [estado]] ++ (drop (contador)(head nLista))
        
    else 
      cambiarEstadoBiciAux (tail nLista) identificador estado (contador+1)

--Funcion encargada de retornar un alquiler
alquilar :: IO ()
alquilar = do
  putStrLn "\t\t<<<<Opciones de alquilar>>>>"
  file <- obtenerLineas "./datos/alquiler.csv"
  putStrLn "\t\tElija un parqueo"
  nParqueo <- getLine

  --Llamar a funcion que muestra la bicicletas por parqueo
  buscarPorParqueo "./datos/bicicletas.csv" nParqueo


  putStrLn "\t\tIngrese el identificador de la bicicleta que más le guste)"
  putStr ">>"
  bicicleta <- getLine


  -- alquilar bicileta
  cambiarEstadoBici bicicleta "activo"

  putStrLn "\t\t Ingrese el nombre del parqueo salida)"
  putStr ">>"
  salida <- getLine
  
  putStrLn "\t\t Ingrese el nombre del parque de llegada)"
  putStr ">>"
  llegada <- getLine

  putStrLn "\t\t Ingrese su cedula de identificacion)"
  putStr ">>"
  codigoAlquilador <- getLine

  let estdoDelAlquiler = "activo"
  --codigoAlquiler <- bicicleta++salida++llegada++codigoAlquilador
  -- llamar a funcion de modificar bicicleta
  agregarFila "./datos/alquiler.csv" ([bicicleta++salida++llegada++codigoAlquilador, bicicleta, salida, llegada, codigoAlquilador,estdoDelAlquiler])
  putStrLn "El alquiler fue todo un exito, disfrute de su viaje!!!"




-- Menu de la información general del la aplicación
mainMenu :: IO ()
mainMenu = do
  putStrLn "\t\tMenú General)"
  putStrLn "1- Consultar bicicletas"
  putStrLn "2- Alquilar"
  putStrLn "3- Facturar"
  putStrLn "4- Salir"
  putStr ">>"
  option <- getLine
  case option of
    "1" -> putStrLn "opcion no disponible..."
    "2" ->  alquilar 
    "3" ->  facturar
    "4" -> return ()
    _ -> putStrLn "[ERROR]: La opción elegida no es válida"
  Control.Monad.when (option /= "4") mainMenu
-------------------------------------Tim

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
  putStrLn "2- Menu general"
  putStrLn "3- Salir"
  option <- getLine
  case option of  
    "1" -> auntentificacion parqueos bicicletas usuarios
    "2" -> mainMenu
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
