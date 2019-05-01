
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
  content <- readFile "pastor.txt"
  let ls = words content
  mostrarTauler partida
  mostrarEntrada partida ls
  --let partida = ferJugades partida ls
  mostrarTauler partida






mostrarTauler :: Partida -> IO()
mostrarTauler (Partida tauler color) = print tauler

mostrarEntrada ::Partida -> [String] -> IO()
mostrarEntrada part@(Partida t c) [] = print "mostrarEntrada: sha entrat a llista buida"
mostrarEntrada part@(Partida t c) [x,y]  --mirar si certament el primer és escac mat
  | y!!5=='+' && y!!6=='+' && escacIMat t c = print "mostrarEntrada: sha entrat com a escac mat"
  | otherwise = print "mostrarEntrada: s'ha entrar com a no Escac mat"
mostrarEntrada part@(Partida t c) (x:y:z:xs)
  | not (isNumber (x!!0) && isLetter (y!!0) && isLetter (z!!0)) = print "mostrarEntrada: sha entrat a llista plena, not number"
  | y!!5=='+' && y!!6=='+' && escacIMat t c = print "mostrarEntrada: sha entrat a llista plena, escacmat"
  | otherwise = print "mostrarEntrada: sha entrat a llista plena, otherwise"


castingChar :: String -> Char
castingChar s = s!!0

ferJugades :: Partida -> [String] -> Partida
ferJugades part@(Partida t c) [] = part
ferJugades part@(Partida t c) [x,y]  --mirar si certament el primer és escac mat, unica possibilitat
  | y!!5=='+' && y!!6=='+' && escacIMat t c = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
  | otherwise = error "tipus de moviment no vàlid, escac mat no existeix"
ferJugades part@(Partida t c) [x,y,z]  --mirar si certament el primer és escac (a seques)
  | y!!5=='+' && escac t c = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
  | otherwise = error "tipus de moviment no vàlid, escac no existeix"
ferJugades part@(Partida t c) (x:y:z:xs) =  ferJugades (iferJugada (iferJugada part y) z) xs
 where
  iferJugada (partida@(Partida t c)) str
    | not (isNumber (x!!0) && isLetter (y!!0) && isLetter (z!!0)) = error "El fitxer conté una jugada mal entrada"
    | y!!5=='+' && y!!6=='+' && escacIMat t c = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
    | otherwise = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)


{- 
| not (isNumber (x!!0) && isLetter (y!!0) && isLetter (z!!0))  = error "El fitxer conté una jugada mal entrada"
  | otherwise = error "El fitxer conté una jugada mal entrada" -}






{- ferJugades partida (x:y:z:xs) = execJugadaPrimer . execJugadaSegona . mostrarTaulerJugada . ferJugadesSeguent(bucle)
  where execJugada 
    |    

    |    -- si jaque inexistent
    |    -- si jaque mate inexistent

    |    -- si jaque existent
    |    -- si jaque mate existent

    |    -- ??algun altre
    |    -- aplicar jugada normal (error es mostra en la propia jugada)
 -}
   
