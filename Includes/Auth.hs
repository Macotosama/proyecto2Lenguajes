module Includes.Auth
(
    confirmarUsuario
)
where

confirmarUsuario :: [Char] -> [Char] -> Bool
confirmarUsuario use pass = use == "Yucosita" && pass == "12345"


