module Jugada (
 Jugada(..)
) where

import Peca
import Posicio

-- Definim la Jugada, com la peça té l'origen només necessitem el destí
data Jugada = Jugada Peca Posicio