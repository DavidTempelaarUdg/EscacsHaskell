module Tauler (
 Tauler,
 taulerInicial,
 pecaA,
 casellaBuida,
 fesJugada,
 jugadaLegal
) where

import Data.Char

import Posicio
import Color
import Peca
import Jugada

-- Definim el tauler d'escacs, és una llista de peçes
data Tauler = Tauler [Peca]

-- Override del show per mostrar el tauler com desitgem
instance Show Tauler where
 show tauler = "\n" ++ iMostrarTauler tauler
  where 
   extremSuperiorIInferior = "   ==========\n"
   mostrarPecaA tauler f c = show (pecaA tauler (f,c))
   mostrarFila tauler f =
    show f ++ "- |" ++
    concat [mostrarPecaA tauler x f|x<-[1..8]] ++
    "|\n"
   iMostrarTauler tauler =
    extremSuperiorIInferior ++
    concat [mostrarFila tauler x|x<-[8,7..1]] ++
    extremSuperiorIInferior ++
    "    abcdefgh\n"

-- Inicialització d'un tauler amb les peces a les posicions inicials
taulerInicial :: Tauler
taulerInicial = Tauler
  ([Peca Blanc P (x,2)|x<-[1..8]] ++
  [Peca Negre P (x,7)|x<-[1..8]] ++
  zipWith (Peca Blanc) (map (\l->read[l]) "TCADRACT") [(i,1)|i<-[1..8]] ++
  zipWith (Peca Negre) (map (\l->read[l]) "TCADRACT") [(i,8)|i<-[1..8]])

-- Funció que ens retorna la peça que ocupa una posició en un tauler
-- En cas de que aquesta estigui buida retorna una peça especial Buida
pecaA :: Tauler -> Posicio -> Peca
pecaA (Tauler llistaPeces) pos 
 | valida pos = trobarPeca llistaPeces pos
 | otherwise = error "posició invàlida"
 where
  trobarPeca (x@(Peca c t pos):xs) p
   | pos == p = x
   | otherwise = trobarPeca xs p
  trobarPeca [] pos = Buida
 
-- Funció que ens retorna cert si la casella està buida en un tauler i una posició, fals altrament
casellaBuida :: Tauler -> Posicio -> Bool
casellaBuida tauler pos = pecaA tauler pos == Buida

-- Funció que rep un tauler, una posició origen i una posició destí i retorna un tauler si la Jugada és legal
-- altrament llença una excepció
fesJugada :: Tauler -> Jugada -> Tauler
fesJugada tauler mov@(Jugada peca posDesti)
 | jugadaLegal tauler mov = iMoure tauler peca posDesti
 | otherwise = error"Jugada il·legal"
 where
  eliminarPeca llistaPeces posD = filter (\x@(Peca c t pos) -> pos /= posD) llistaPeces
  modificarPosicioPeca llistaPeces posD peca@(Peca c t pos) = Tauler ((eliminarPeca llistaPeces pos) ++ [(Peca c t posD)])
  iMoure tauler@(Tauler llistaPeces) peca@(Peca color tipus posOrigen) posDesti
   | pecaA tauler posDesti /= Buida = modificarPosicioPeca (eliminarPeca llistaPeces posDesti) posDesti peca
   | otherwise = modificarPosicioPeca llistaPeces posDesti peca
   
-- Funció que ens diu si una jugada és legal en un taler. Retorna cert si ho és, fals altrament
jugadaLegal :: Tauler -> Jugada -> Bool
jugadaLegal t mov = True