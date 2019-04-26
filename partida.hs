module Partida(
 Partida,
 iniciPartida,
 canviJugador
) where

import Tauler
import Color

-- Definim la partida, és un tauler i qui juga
-- L'estat inicial de la partida, sempre comencen blanques
type Partida = (Tauler, Color)

iniciPartida :: Partida
iniciPartida = (taulerInicial, Blanc)

canviJugador :: Partida -> Partida
canviJugador (t, c) = (t, contrari c)