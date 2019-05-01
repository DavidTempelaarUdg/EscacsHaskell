
import Data.Char
import Control.Monad
import Jugada
import Partida
import Posicio
import Peca
import Color
import Tauler
import System.IO (hFlush, stdout)



main = do
  let partida = iniciPartida
  content <- readFile "pastor_copia.txt"
  let ls = words content
  mostrarTauler partida
  mostrarEntrada partida ls

  let loop = do
      let partidaI = ferJugades partida ls

      let partida = fst partidaI
      mostrarTauler partida

      let ls = snd partidaI

      mostrarEntrada partida ls

      let seguir = quedenParaules ls
      when (seguir /= 'n') loop
  loop 



--fst snd

mostrarTauler :: Partida -> IO()
mostrarTauler (Partida tauler color) = print tauler

quedenParaules :: [String] -> Char
quedenParaules [] = 'n'
quedenParaules x = 'y'

ferJugades :: Partida -> [String] -> (Partida,[String])
ferJugades part@(Partida t c) [] = (part,[])
ferJugades part@(Partida t c) [x,y] = ((Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((toInteger(digitToInt (y!!2))),(taulerCharToInteger (y!!1)))) (((toInteger(digitToInt (y!!4))),(taulerCharToInteger (y!!3)))))) (contrari c)),[])
ferJugades part@(Partida t c) (x:y:z:xs) = ((Partida (fesJugada (jugAnterior part y) (Jugada (Peca (contrari c) (((\l->read[l]) (z!!0))) ((taulerCharToInteger (z!!1)),(toInteger(digitToInt (z!!2))))) (((taulerCharToInteger (z!!3)),(toInteger(digitToInt (z!!4))))))) c),xs)
 where
  jugAnterior (partida@(Partida t c)) n = fesJugada t (Jugada (Peca c (((\l->read[l]) (n!!0))) ((toInteger(digitToInt (n!!2))),(taulerCharToInteger (n!!1)))) (((toInteger(digitToInt (n!!4))),(taulerCharToInteger (n!!3))))) 




mostrarEntrada :: Partida -> [String] -> IO()
mostrarEntrada part@(Partida t c) [] = print "mostrarEntrada: sha entrat a llista buida"
mostrarEntrada part@(Partida t c) [x,y] = print "mostrarEntrada: s'ha entrat llista 2"
mostrarEntrada part@(Partida t c) [x,y,z] = print "mostrarEntrada: s'ha entrat llista 3"
mostrarEntrada part@(Partida t c) (x:y:z:xs) = print "mostrarEntrada: sha entrat a llista plena, otherwise"










































{- main = do
  let partida = iniciPartida
  content <- readFile "pastor_copia.txt"
  let ls = words content
  
  mostrarTauler partida

  let loop = do

      let jugadaStr = fstStr ls
      let lsI = extStr ls
      mapM_ putStrLn lsI


      let seguir = quedenParaules lsI
      when (seguir /= 'n') loop
  loop 



--fst snd

mostrarTauler :: Partida -> IO()
mostrarTauler (Partida tauler color) = print tauler

fstStr :: [String] -> String
fstStr [] = []
fstStr [x] = x
fstStr (x:xs) = x

extStr :: [String] -> [String]
extStr [] = []
extStr [x] = []
extStr (x:xs) = xs

quedenParaules :: [String] -> Char
quedenParaules [] = 'n'
quedenParaules x = 'y'

getStrJugada :: [String] -> (String,[String])
getStrJugada [] = ([],[])
getStrJugada (x:xs)
  | isNumber (x!!0) = ([],xs)
  | otherwise = (x,xs)
 -}




  
{- ferJugades :: Partida -> [String] -> (Partida,[String])
ferJugades part@(Partida t c) [] = (part,[])
ferJugades part@(Partida t c) [x,y] = ((Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((toInteger(digitToInt (y!!2))),(taulerCharToInteger (y!!1)))) (((toInteger(digitToInt (y!!4))),(taulerCharToInteger (y!!3)))))) (contrari c)),[])
ferJugades part@(Partida t c) (x:y:z:xs) = 
  -}


{- mostrarEntrada :: Partida -> [String] -> IO()
mostrarEntrada part@(Partida t c) [] = print "mostrarEntrada: sha entrat a llista buida"
mostrarEntrada part@(Partida t c) [x,y] = print "mostrarEntrada: s'ha entrat llista 2"
mostrarEntrada part@(Partida t c) [x,y,z] = print "mostrarEntrada: s'ha entrat llista 3"
mostrarEntrada part@(Partida t c) (x:y:z:xs) = print "mostrarEntrada: sha entrat a llista plena, otherwise"
 -}

{- ferJugades part@(Partida t c) [] = (part,[])
ferJugades part@(Partida t c) [x,y]  --mirar si certament el primer és escac mat, unica possibilitat
  | y!!5=='+' && y!!6=='+' && escacIMat t c = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
  | otherwise = error "tipus de moviment no vàlid, escac mat no existeix"
ferJugades part@(Partida t c) [x,y,z]  --mirar si certament el segon és escac mat, unica possibilitat
  | z!!5=='+' && z!!6=='+' && (escacIMat t (contrari c)) = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
  | otherwise = error "tipus de moviment no vàlid, escac mat no existeix"
ferJugades part@(Partida t c) (x:y:z:xs) =  ferJugades (iferJugada (iferJugada part y) z) xs
 where
  iferJugada (partida@(Partida t c)) str
    | not (isNumber (x!!0) && isLetter (y!!0) && isLetter (z!!0)) = error "El fitxer conté una jugada mal entrada"
    | (any ('+'==) y) && y!!5=='+' && escac t c = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
    | otherwise = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
  -}



{- 
mostrarEntrada ::Partida -> [String] -> IO()
mostrarEntrada part@(Partida t c) [] = print "mostrarEntrada: sha entrat a llista buida"
mostrarEntrada part@(Partida t c) [x,y]  --mirar si certament el primer és escac mat
  | y!!5=='+' && y!!6=='+' && escacIMat t c = print "mostrarEntrada: sha entrat com a escac mat"
  | otherwise = print "mostrarEntrada: s'ha entrar com a no Escac mat"
mostrarEntrada part@(Partida t c) [x,y,z]  --mirar si certament el segon és escac mat
  | z!!5=='+' && z!!6=='+' && escacIMat t (contrari c) = print "mostrarEntrada: sha entrat com a escac mat contrincant"
  | otherwise = print "mostrarEntrada: s'ha entrar com a no Escac mat contrincant"
mostrarEntrada part@(Partida t c) (x:y:z:xs)
  | not (isNumber (x!!0) && isLetter (y!!0) && isLetter (z!!0)) = print "mostrarEntrada: sha entrat a llista plena, not number"
  | (any ('+'==) y) && y!!5=='+' && escac t c = print "mostrarEntrada: sha entrat a llista plena, escac mat"
  | otherwise = print "mostrarEntrada: sha entrat a llista plena, otherwise"

 -}




{- 
ferJugades part@(Partida t c) [x,y,z]  --mirar si certament el primer és escac (a seques)
  | y!!5=='+' && escac t c = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
  | otherwise = error "tipus de moviment no vàlid, escac no existeix"
ferJugades part@(Partida t c) (x:y:z:xs) =  ferJugades (iferJugada (iferJugada part y) z) xs
 where
  iferJugada (partida@(Partida t c)) str
    | not (isNumber (x!!0) && isLetter (y!!0) && isLetter (z!!0)) = error "El fitxer conté una jugada mal entrada"
    | y!!5=='+' && y!!6=='+' && escacIMat t c = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
    | otherwise = Partida (fesJugada t (Jugada (Peca c (((\l->read[l]) (y!!0))) ((taulerCharToInteger (y!!1)),(toInteger(digitToInt (y!!2))))) (((taulerCharToInteger (y!!3)),(toInteger(digitToInt (y!!4))))))) (contrari c)
 -}

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
   
