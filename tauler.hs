module Tauler (
 Tauler,
 taulerBuit,
 taulerInicial,
 pecesDeUnColor,
 pecaA,
 posicioRey,
 casellaBuida,
 eliminarPeca,
 afegirPeca,
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
    concat [mostrarPecaA tauler f x|x<-[1..8]] ++
    "|\n"
   iMostrarTauler tauler =
    extremSuperiorIInferior ++
    concat [mostrarFila tauler x|x<-[8,7..1]] ++
    extremSuperiorIInferior ++
    "    abcdefgh\n"

-- Inicialització d'un tauler buit
taulerBuit :: Tauler
taulerBuit = Tauler []

-- Inicialització d'un tauler amb les peces a les posicions inicials
taulerInicial :: Tauler
taulerInicial = Tauler
  ([Peca Blanc P (2,x)|x<-[1..8]] ++
  [Peca Negre P (7,x)|x<-[1..8]] ++
  zipWith (Peca Blanc) (map (\l->read[l]) "TCADRACT") [(1,i)|i<-[1..8]] ++
  zipWith (Peca Negre) (map (\l->read[l]) "TCADRACT") [(8,i)|i<-[1..8]])

-- Funció que donat un tauler i un color retorna una llista amb totes les peces d'aquell color
pecesDeUnColor :: Tauler -> Color -> [Peca]
pecesDeUnColor (Tauler llistaPeces) color = filter (\x@(Peca c t pos) -> c == color) llistaPeces

-- Funció que ens retorna la peça que ocupa una posició en un tauler
-- En cas de que aquesta estigui buida retorna una peça especial Buida
pecaA :: Tauler -> Posicio -> Peca
pecaA (Tauler llistaPeces) pos 
 | valida pos = trobarPeca llistaPeces pos
 | otherwise = Buida
 where
  trobarPeca (x@(Peca c t pos):xs) p
   | pos == p = x
   | otherwise = trobarPeca xs p
  trobarPeca [] pos = Buida
  
-- Funció que rep un tauler i un color i ens diu la posició en la que es troba aquell rei
posicioRey :: Tauler -> Color -> Posicio
posicioRey (Tauler llistaPeces) color = iPosicioRey llistaPeces color
 where 
  iPosicioRey (x@(Peca c t pos):xs) color
   | t== R && c == color = pos
   | otherwise = iPosicioRey xs color

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
-- S'ha de fer servir pels moviments de torre, alfil i dama
alguEntre :: Tauler -> Posicio -> Posicio -> Bool
alguEntre tauler posOrigen@(x_ori, y_ori) posDesti@(x_des, y_des)
 | x_ori - x_des == 0 = alguEntreVertical tauler posOrigen y_des
 | y_ori - y_des == 0 = alguEntreHoritzontal tauler posOrigen x_des
 | abs(x_ori - x_des) == abs(y_ori - y_des) = alguEntreDiagonal tauler posOrigen posDesti
 | otherwise = error "tipus de moviment no vàlid"
 where
  alguEntreVertical tauler posOri@(x,y_inicial) y_final
   | y_inicial<=y_final = any (/=True) [casellaBuida tauler (x,y) | y <- [(y_inicial+1)..(y_final-1)]]
   | otherwise = any (/=True) [casellaBuida tauler (x,y) | y <- [(y_final+1)..(y_inicial-1)]]
  alguEntreHoritzontal tauler posOri@(x_inicial,y) x_final
   | x_inicial<=x_final = any (/=True) [casellaBuida tauler (x,y) | x <- [(x_inicial+1)..(x_final-1)]]
   | otherwise = any (/=True) [casellaBuida tauler (x,y) | x <- [(x_final+1)..(x_inicial-1)]]
  alguEntreDiagonal tauler posOri@(x_inicial,y_inicial) posDesti@(x_final,y_final)
   -- Les dos creixen
   | x_inicial<=x_final && y_inicial<=y_final = any (/=True) [casellaBuida tauler (x_inicial+x,y_inicial+x) | x <- [1..(x_final-x_inicial)]]
   -- Les dos disminueixen
   | x_inicial>x_final && y_inicial>y_final = any (/=True) [casellaBuida tauler (x_inicial-x,y_inicial-x) | x <- [1..(x_inicial-x_final)]]
   -- Creix x, però no y
   | x_inicial<=x_final && y_inicial>y_final = any (/=True) [casellaBuida tauler (x_inicial+x,y_inicial-x) | x <- [1..(x_final-x_inicial)]]
   -- No creix x, però sí y
   | otherwise = any (/=True) [casellaBuida tauler (x_inicial-x,y_inicial+x) | x <- [1..(x_inicial-x_final)]]