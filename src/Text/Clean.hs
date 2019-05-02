module Text.Clean
    (removeAccents)
    where

import Data.Text as T

removeAccents :: Text -> Text
removeAccents = T.map unAccent

unAccent :: Char -> Char
unAccent 'Á' = 'A'
unAccent 'É' = 'E'
unAccent 'Í' = 'I'
unAccent 'Ó' = 'O'
unAccent 'Ú' = 'U'
unAccent 'Ü' = 'U'
unAccent 'Ñ' = 'N'
unAccent 'á' = 'a'
unAccent 'é' = 'e'
unAccent 'í' = 'i'
unAccent 'ó' = 'o'
unAccent 'ú' = 'u'
unAccent 'ü' = 'u'
unAccent 'ñ' = 'n'
unAccent x   = x
