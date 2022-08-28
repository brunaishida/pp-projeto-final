module Utils where
import Data.List

intToString :: Int -> String
intToString n = show n

stringToInt :: String -> Int
stringToInt n = read n ::Int

integerToString :: Integer -> String
integerToString n = show n

integerToInt :: Integer -> Int
integerToInt n = fromInteger n

sortGT :: (Int, String) -> (Int, String) -> Ordering
sortGT (a1, b1) (a2, b2)
  | a1 < a2 = GT
  | a1 > a2 = LT
  | a1 == a2 = compare b1 b2

ordenaPeloPrimeiroDaTupla :: [(Int, String)] -> [(Int, String)]
ordenaPeloPrimeiroDaTupla = sortBy sortGT

somaPontosLance :: (Int, Int) -> Int
somaPontosLance tuplePontuacao = (fst tuplePontuacao) + (snd tuplePontuacao)

validaResultadoRodada :: (Int,Int) -> String
validaResultadoRodada resultado = if (fst resultado == 10) then "Strike"
  else 
    if (((fst resultado) + (snd resultado)) == 10) then "Spare"
    else "Normal"

puroJust :: Maybe a -> a
puroJust (Just a) = a
