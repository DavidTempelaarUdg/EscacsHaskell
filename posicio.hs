module Posicio (
 Posicio,
 valida,
 taulerCharToInteger
) where

-- Definim un sinónim per les coordenades d'una posició en el tauler
-- La posició (1,1) és l'inferior esquerre de les blanques i la (1,8) és la cantonada inferior dreta de les blanques
type Posicio = (Integer, Integer)

-- Funció que ens retorna cert si una posició és vàlida del tauler
valida :: Posicio -> Bool
valida (f, c) = iValida f && iValida c
 where 
  iValida x = x >= 1 && x <= 8

-- Funció que ens retorna el número que relaciona la lletra de l'abecedari del tauler  
taulerCharToInteger :: Char -> Integer
taulerCharToInteger ch
  | ch == 'a' = 1
  | ch == 'b' = 2
  | ch == 'c' = 3
  | ch == 'd' = 4
  | ch == 'e' = 5
  | ch == 'f' = 6
  | ch == 'g' = 7
  | ch == 'h' = 8