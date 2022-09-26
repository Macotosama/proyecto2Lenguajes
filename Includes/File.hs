module Includes.File (
    split,
    convertirString,
    fileToMatrix,
    leerArchivo,
    obtenerArchivo
)
where
import System.IO (withFile, IOMode (ReadMode), hGetContents)

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
fileToMatrix :: [[Char]] -> [[[Char]]]
fileToMatrix list = do
  if null list
    then [[]]
    else convertirString list []

--Lee un archivo por medio de su ruta
leerArchivo :: FilePath -> IO [Char]
leerArchivo ruta = withFile ruta ReadMode $ \handle -> do
    theContent <- hGetContents handle
    mapM return theContent

--Busca el archivo y regresa los datos del archivo en un archivo
obtenerArchivo ruta = do
    archivo <- leerArchivo ruta
    return (fileToMatrix archivo)