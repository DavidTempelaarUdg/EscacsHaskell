module Tauler (
 Tauler,
 taulerInicial,
 pecaA,
 casellaBuida,
 eliminarPeca,
 modificarPosicioPeca,
 alguEntre
) where

import Data.Char

import Posicio
import Color
import Peca

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
 | otherwise = error "Has intentat buscar una peça en una posició que no és vàlida"
 where
  trobarPeca (x@(Peca c t pos):xs) p
   | pos == p = x
   | otherwise = trobarPeca xs p
  trobarPeca [] pos = Buida

-- Funció que rep un tauler i una posició i retorna el tauler sense la peça que ocupava aquella posició
eliminarPeca :: Tauler -> Posicio -> Tauler
eliminarPeca (Tauler llistaPeces) posD
 | valida posD = Tauler (filter (\x@(Peca c t pos) -> pos /= posD) llistaPeces)
 | otherwise = error "Has intentat eliminar una peça d'una posició que no és correcte"

-- Funció que rep un tauler i una peça i retorna un tauler amb la peça afegida en aquesta
afegirPeca :: Tauler -> Peca -> Tauler
afegirPeca (Tauler llistaPeces) peca = Tauler (llistaPeces ++ [peca])

-- Funció que rep un tauler, una peça i una posició destí i retorna un tauler amb la peça moguda a destí
modificarPosicioPeca :: Tauler -> Peca -> Posicio -> Tauler
modificarPosicioPeca tauler@(Tauler llistaPeces)  peca@(Peca c t pos) posD 
 | valida posD && peca == pecaA tauler pos = afegirPeca (eliminarPeca tauler pos) (Peca c t posD)
 | otherwise = error "Has intentat moure una peça a una posició que no existeix o la peça que vols moure no ocupa aquella posició"

-- Funció que ens retorna cert si la casella està buida en un tauler i una posició, fals altrament
casellaBuida :: Tauler -> Posicio -> Bool
casellaBuida tauler pos = pecaA tauler pos == Buida

-- Funció que rep un tauler i dos posicions i retorna cert si hi ha peces entre les dos posicions o fals altrament
alguEntre :: Tauler -> Posicio -> Posicio -> Bool
alguEntre tauler posOrigen posDesti = False

