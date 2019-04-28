module Posicio (
 Posicio,
 valida,
 posicionsDesde,
 posicionsDesdeCavall
) where

-- Definim un sinónim per les coordenades d'una posició en el tauler
-- La posició (1,1) és l'inferior esquerre de les blanques i la (1,8) és la cantonada inferior dreta de les blanques
type Posicio = (Integer, Integer)

-- Funció que ens retorna cert si una posició és vàlida del tauler
valida :: Posicio -> Bool
valida (f, c) = iValida f && iValida c
 where 
  iValida x = x >= 1 && x <= 8

-- Funció que rep una posició, una profunditat i una llista de direccions i retorna una llista de posicions vàlides on una peça
-- es pot moure linealment. La peça ha de ser Rei, Dama, Alfil o Torre i considera que el tauler està buit.
posicionsDesde :: Posicio -> Integer -> [(Posicio)] -> [(Posicio)]
posicionsDesde pos@(x,y) profunditat direccions = filter (valida) [(x+(d_x*z),y+(d_y*z)) | (d_x,d_y) <- direccions, z <-[1..profunditat]]

-- Funció que rep una posició i una llista de direccions i retorna una llista de posicions vàlides al que podria moure un Cavall.
-- Considera que el tauler està buit.
posicionsDesdeCavall :: Posicio -> [(Posicio)] -> [(Posicio)]
posicionsDesdeCavall (x,y) direccions = filter (valida) [(x+d_x,y+d_y) | (d_x,d_y) <- direccions]