module Peca (
 TipusDePeca (P, C, A, T, D, R),
 Peca (..),
 posicioInicial,
 sonDelMateixColor
) where

import Data.Char

import Color
import Posicio

-- Definim els tipus de peça
data TipusDePeca = P | C | A | T | D | R deriving (Read, Eq)

-- Override del show per mostrar el tipus de la peça
instance Show TipusDePeca where
 show P = "p"
 show C = "c"
 show A = "a"
 show T = "t"
 show D = "d"
 show R = "r"
 
-- Definim la peça
data Peca = Peca Color TipusDePeca Posicio | Buida deriving (Eq)

--  Override del show per modificar com es mostra, blanques en majúscules, negres en minúscules
instance Show Peca where
 show Buida = id "."
 show (Peca Blanc t pos) = map toUpper $ show t
 show (Peca Negre t pos) = show t

-- Funció que rep una peça i retorna cert si es troba a la posició inicial, fals altrament
posicioInicial :: Peca -> Bool
posicioInicial (Peca Blanc P (x,y)) = x == 2
posicioInicial (Peca Negre P (x,y)) = x == 7
posicioInicial (Peca Blanc _ (x,y)) = x == 1
posicioInicial (Peca Negre _ (x,y)) = x == 8

-- Funció que rep dos peces i retorna cert si són del mateix color, fals altrament
sonDelMateixColor :: Peca -> Peca -> Bool
sonDelMateixColor _ Buida = False
sonDelMateixColor Buida _ = False
sonDelMateixColor (Peca c1 _ _) (Peca c2 _ _) = c1 == c2