module Includes.File (
    split,
    convertirString,
    arreglarString,
    obtenerLineas,
    leerArchivo,
    obtenerArchivo,
    filasString,
    listaString,
    escribirArchivo
) where
import System.IO (withFile, IOMode (ReadMode), hGetContents)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (sort,group)
import Data.IORef

-- Separa un string si es igual a un token
split :: (Char -> Bool) -> [Char] -> [[Char]]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where
      (w, s'') = break p s'

-- Función auxiliar que crea una matriz separando de los strings por coma
convertirString :: [[Char]] -> [[[Char]]] -> [[[Char]]]
convertirString list newList = do
  if null list
    then newList
    else convertirString (init list) newList ++ [split (== ',') (last list)]

-- Convierte una lista de strings y las convierte en una matriz
arreglarString :: [[Char]] -> [[[Char]]]
arreglarString list = do
  if null list
    then [[]]
    else convertirString list []

--Lee un archivo por medio de su ruta
leerArchivo :: FilePath -> IO [Char]
leerArchivo ruta = withFile ruta ReadMode $ \handle -> do
  theContent <- hGetContents handle
  mapM return theContent

obtenerLineas :: FilePath -> IO [String] --Lista de las lineas del archivo
obtenerLineas path = do
  str <- leerArchivo path
  return (lines str)

--Busca el archivo y regresa los datos del archivo en un archivo
obtenerArchivo :: FilePath -> IO [[[Char]]]
obtenerArchivo ruta = do
    archivo <- obtenerLineas ruta
    return (arreglarString archivo)

--Convierte una lista a un string
filasString :: [[Char]] -> [Char]
filasString list
  | null list = return '\n'
  | length list == 1 = head list++filasString (tail list)
  | otherwise = head list++","++filasString (tail list)

--Convierte una matriz a string
listaString :: [[[Char]]] -> [Char]
listaString matrix = do
    if null matrix then
      ""
    else filasString (head matrix) ++ listaString (tail matrix)

--Elimina todo el contenido de un archivo y agrega nueva información
escribirArchivo :: FilePath -> [[[Char]]] -> IO ()
escribirArchivo path newData = do
  let newDataStr = listaString newData
  writeFile path newDataStr