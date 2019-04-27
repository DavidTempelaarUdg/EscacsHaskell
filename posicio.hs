module Posicio (
 Posicio,
 valida
) where

-- Definim un sinónim per les coordenades d'una posició en el tauler
-- La posició (1,1) és l'inferior esquerre de les blanques i la (1,8) és la cantonada inferior dreta de les blanques
type Posicio = (Integer, Integer)

-- Funció que ens retorna cert si una posició és vàlida del tauler
valida :: Posicio -> Bool
valida (f, c) = iValida f && iValida c
 where 
  iValida x = x >= 1 && x <= 8