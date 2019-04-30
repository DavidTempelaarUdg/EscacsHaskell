
import Jugada
import Partida
import Posicio
import Peca
import Color
import Tauler
import System.IO (hFlush, stdout)


main = do
  let partidaI = iniciPartida
  content <- readFile "prova.txt"
  let ls = words content
  mostrarTauler partidaI
  let ns = ["a","b","c"]
  let p = ferJugades partidaI ns
  mostrarTauler p


mostrarTauler :: Partida -> IO()
mostrarTauler (t,c) = print t

ferJugades :: Partida -> [String] -> Partida
ferJugades (t,c) [] = (t,c)
ferJugades (t,c) [x,y]  --mirar si certament el primer és escac mat
  | escacIMat t c = (fesJugada t (Jugada (Peca c (llegirTipus y!!0) (Posicio ......)) (Posicio)), contrari c)
  | otherwise = error "tipus de moviment no vàlid, escac mat on no existeix"
ferJugades (t,c) (x:y:z:xs) = (t,c)

--quan en un cicle hi ha escac mat i no juga laltre jugador

{- ferJugades partida (x:y:z:xs) = execJugada . mostrarTauler partida . ferJugades
  where execJugada 
    |    -- = error "El fitxer conté una jugada mal entrada"  --mal entrat
    |    -- si jaque inexistent
    |    -- si jaque existent
    |    -- si jaque mate inexistent 
    |    -- si jaque mate existent
    |    -- ??algun altre
    |    -- aplicar jugada normal (error es mostra en la propia jugada)
 -}
{- fesJugada :: Tauler -> Jugada -> Tauler
fesJugada tauler mov@(Jugada peca@(Peca c t pos) posDesti)
 | peca == (pecaA tauler pos) && jugadaLegal tauler mov = iMoure tauler peca posDesti
 | otherwise = error"Jugada il·legal"
 where
  iMoure tauler peca@(Peca color tipus posOrigen) posDesti
   | pecaA tauler posDesti /= Buida = modificarPosicioPeca (eliminarPeca tauler posDesti) peca posDesti
   | otherwise = modificarPosicioPeca tauler peca posDesti -}
   
