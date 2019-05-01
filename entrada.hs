
import Data.Char

import Jugada
import Partida
import Posicio
import Peca
import Color
import Tauler
import System.IO (hFlush, stdout)




main = do
  let partida = iniciPartida
  content <- readFile "prova.txt"
  let ls = words content
  mapM_ putStrLn ls
  mostrarTauler partida
  mostrarEntrada partida ls
  let partida = ferJugades partida ls
  mostrarTauler partida






mostrarTauler :: Partida -> IO()
mostrarTauler (Partida tauler color) = print tauler

mostrarEntrada ::Partida -> [String] -> IO()
mostrarEntrada part@(Partida t c) [] = print "mostrarEntrada: sha entrat a llista buida"
mostrarEntrada part@(Partida t c) [x,y]  --mirar si certament el primer és escac mat
  | y!!5=='+' && y!!6=='+' && escacIMat t c = print "mostrarEntrada: sha entrat com a escac mat"
  | otherwise = print "mostrarEntrada: s'ha entrar com a no Escac mat"
mostrarEntrada part@(Partida t c) (x:y:z:xs) = print "mostrarEntrada: te mes de dos entrades a string"

castingChar :: String -> Char
castingChar s = s!!0

ferJugades :: Partida -> [String] -> Partida
ferJugades part@(Partida t c) [] = part
ferJugades part@(Partida t c) [x,y]  --mirar si certament el primer és escac mat
  | y!!5=='+' && y!!6=='+' && escacIMat t c = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
  | otherwise = error "tipus de moviment no vàlid, escac mat on no existeix"
ferJugades part@(Partida t c) (x:y:z:xs) = part





{- 
  fesJugada :: Tauler -> Jugada -> Tauler
  Jugada Peca Posicio
  Peca Color TipusDePeca Posicio
  Posicio = (Integer, Integer)

  fesJugada t (Jugada (Peca c (read y!!0) ((taulerCharToInteger y!!1),())) (posDesti))








  
ferJugades part@(Partida t c) (x:y:z:xs) = part -}

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
   
