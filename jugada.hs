module Jugada (
 Jugada(..),
 fesJugada,
 jugadaLegal
) where

import Posicio
import Color
import Peca
import Tauler

-- Definim la Jugada, com la peça té l'origen només necessitem el destí
data Jugada = Jugada Peca Posicio

-- Funció que rep un tauler, una posició origen i una posició destí i retorna un tauler si la Jugada és legal
-- altrament llença una excepció
fesJugada :: Tauler -> Jugada -> Tauler
fesJugada tauler mov@(Jugada peca posDesti)
 | jugadaLegal tauler mov = iMoure tauler peca posDesti
 | otherwise = error"Jugada il·legal"
 where
  iMoure tauler@(Tauler llistaPeces) peca@(Peca color tipus posOrigen) posDesti
   | pecaA tauler posDesti /= Buida = modificarPosicioPeca (eliminarPeca llistaPeces posDesti) posDesti peca
   | otherwise = modificarPosicioPeca llistaPeces posDesti peca
   
-- Funció que ens diu si una jugada és legal en un taler. Retorna cert si ho és, fals altrament
jugadaLegal :: Tauler -> Jugada -> Bool
jugadaLegal t mov = True