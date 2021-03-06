module Jugada (
 Jugada(..),
 fesJugada,
 jugadaLegal,
 movimentLegal,
 moviment,
 posicionsDesde,
 posicionsDesdeCavall,
 escac,
 escacIMat,
 movimentLegalNoComplet
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
fesJugada tauler mov@(Jugada peca@(Peca c t pos) posDesti)
 | peca == (pecaA tauler pos) && jugadaLegal tauler mov = iMoure tauler peca posDesti
 | otherwise = error"Jugada il·legal"
 where
  iMoure tauler peca@(Peca color tipus posOrigen) posDesti
   | pecaA tauler posDesti /= Buida = modificarPosicioPeca (eliminarPeca tauler posDesti) peca posDesti
   | otherwise = modificarPosicioPeca tauler peca posDesti
   
-- Funció que ens diu si una jugada és legal en un tauler. Retorna cert si ho és, fals altrament
jugadaLegal :: Tauler -> Jugada -> Bool
jugadaLegal tauler mov@(Jugada peca posDesti) = pertanyAlsMovimentsLegals (movimentLegal tauler peca) posDesti
 where 
  pertanyAlsMovimentsLegals [] posDesti = False
  pertanyAlsMovimentsLegals (x:xs) posDesti = x==posDesti || pertanyAlsMovimentsLegals xs posDesti  

-- Funció que rep un tauler i una peça i retorna totes les posicions on pot arribar, tenint en compte que és el torn del jugador i que no pot acabar amb el rei amenaçat
movimentLegal :: Tauler -> Peca -> [Posicio]
movimentLegal tauler peca@(Peca c t pos) 
 | t == C =  filtrarPosicionsFinalsPerqueElReyEstaAmenacat tauler peca (filtrarPosicionsFinalsPerColor tauler peca (moviment peca))
 | t == P =  filtrarPosicionsFinalsPerqueElReyEstaAmenacat tauler peca (filtrarPosicionsFinalsPerColor tauler peca (filtrarPosicionsFinalsPerNoArribables tauler pos ((moviment peca) ++ (afegirPosicionsDeAtacDePeo tauler peca))))
 | otherwise = filtrarPosicionsFinalsPerqueElReyEstaAmenacat tauler peca (filtrarPosicionsFinalsPerColor tauler peca (filtrarPosicionsFinalsPerNoArribables tauler pos (moviment peca)))
 where 
  filtrarPosicionsFinalsPerqueElReyEstaAmenacat tauler peca posicions = filter (not . elReiEstaAmenacat tauler peca) posicions
  filtrarPosicionsFinalsPerColor tauler peca posicions = filter (not . iSonDelMateixColor tauler peca) posicions
  filtrarPosicionsFinalsPerNoArribables tauler pos posicions = filter (not . alguEntre tauler pos) posicions
  iSonDelMateixColor tauler peca pos = sonDelMateixColor peca (pecaA tauler pos)
  afegirPosicionsDeAtacDePeo tauler peca@(Peca c t pos@(x,y)) = filter (not . iSonDelMateixColor tauler peca) (filter (not . casellaBuida tauler) [((if c==Blanc then x+1 else x-1),y+y_a) | y_a <- [-1,1]])
  elReiEstaAmenacat tauler peca@(Peca c t posO) pos = escac (modificarPosicioPeca tauler peca pos) c

-- Funció que rep una peça i retorna una llista de posicios a la que aquella peça podria anar en un tauler buit
moviment :: Peca -> [Posicio]
moviment peca@(Peca Blanc P (x,y)) = filter (valida) ((if posicioInicial peca then [(x+2,y)] else []) ++ [(x+1,y)])
moviment peca@(Peca Negre P (x,y)) = filter (valida) ((if posicioInicial peca then [(x-2,y)] else []) ++ [(x-1,y)])
moviment (Peca _ R pos) = posicionsDesde pos 1 [(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)]
moviment (Peca _ D pos) = posicionsDesde pos 8 [(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)]
moviment (Peca _ T pos) = posicionsDesde pos 8 [(1,0),(0,1),(-1,0),(0,-1)]
moviment (Peca _ A pos) = posicionsDesde pos 8 [(1,1),(-1,1),(-1,-1),(1,-1)]
moviment (Peca _ C pos) = posicionsDesdeCavall pos [(2,-1),(2,1),(-2,-1),(-2,1),(1,2),(-1,2),(1,-2),(-1,-2)]

-- Funció que rep una posició, una profunditat i una llista de direccions i retorna una llista de posicions vàlides on una peça
-- es pot moure linealment. La peça ha de ser Rei, Dama, Alfil o Torre i considera que el tauler està buit.
posicionsDesde :: Posicio -> Integer -> [(Posicio)] -> [(Posicio)]
posicionsDesde pos@(x,y) profunditat direccions = filter (valida) [(x+(d_x*z),y+(d_y*z)) | (d_x,d_y) <- direccions, z <-[1..profunditat]]

-- Funció que rep una posició i una llista de direccions i retorna una llista de posicions vàlides al que podria moure un Cavall.
-- Considera que el tauler està buit.
posicionsDesdeCavall :: Posicio -> [(Posicio)] -> [(Posicio)]
posicionsDesdeCavall (x,y) direccions = filter (valida) [(x+d_x,y+d_y) | (d_x,d_y) <- direccions]

-- Funció que rep un tauler i un color i ens diu si el rei està amenaçat en el tauler
escac :: Tauler -> Color -> Bool
escac tauler color = any (==(posicioRey tauler color)) (concatMap (movimentLegalNoComplet tauler) (pecesDeUnColor tauler (contrari color)))

-- Funció que rep un tauler i un color i diu si aquell color ha rebut un escac i mat
escacIMat :: Tauler -> Color -> Bool
escacIMat tauler color = (escac tauler color) && (concatMap (movimentLegalNoComplet tauler) (pecesDeUnColor tauler color) == []) 

-- Funció que rep un tauler i una peça i retorna una llista de posicions que la peça pot arribar. No té en compte si el seu moviment posa en perill el rei
movimentLegalNoComplet :: Tauler -> Peca -> [Posicio]
movimentLegalNoComplet tauler peca@(Peca c t pos) 
 | t == C =  (filtrarPosicionsFinalsPerColor tauler peca (moviment peca))
 | t == P =  (filtrarPosicionsFinalsPerColor tauler peca (filtrarPosicionsFinalsPerNoArribables tauler pos ((moviment peca) ++ (afegirPosicionsDeAtacDePeo tauler peca))))
 | otherwise = (filtrarPosicionsFinalsPerColor tauler peca (filtrarPosicionsFinalsPerNoArribables tauler pos (moviment peca)))
 where 
  filtrarPosicionsFinalsPerColor tauler peca posicions = filter (not . iSonDelMateixColor tauler peca) posicions
  filtrarPosicionsFinalsPerNoArribables tauler pos posicions = filter (not . alguEntre tauler pos) posicions
  iSonDelMateixColor tauler peca pos = sonDelMateixColor peca (pecaA tauler pos)
  afegirPosicionsDeAtacDePeo tauler peca@(Peca c t pos@(x,y)) = filter (not . iSonDelMateixColor tauler peca) (filter (not . casellaBuida tauler) [((if c==Blanc then x+1 else x-1),y+y_a) | y_a <- [-1,1]])
