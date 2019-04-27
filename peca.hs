module Peca (
 TipusDePeca (P, C, A, T, D, R),
 Peca (..)
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