module Partida(
 Partida,
 iniciPartida,
 canviJugador
) where

import Color
import Peca
import Tauler

-- Definim la partida, és un tauler i qui juga
-- L'estat inicial de la partida, sempre comencen blanques
data Partida = Partida Tauler Color

iniciPartida :: Partida
iniciPartida = Partida taulerInicial Blanc

canviJugador :: Partida -> Partida
canviJugador (Partida t c) = Partida t (contrari c)