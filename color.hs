module Color (
 Color (Blanc, Negre),
 contrari
) where

-- Definim la data color
data Color = Blanc | Negre deriving (Show, Eq)

-- Funció que ens retorna qui és el següent en jugar
contrari :: Color -> Color
contrari Blanc = Negre
contrari Negre = Blanc