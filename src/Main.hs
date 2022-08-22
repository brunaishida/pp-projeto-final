module Main where
import System.Random
import Data.List

iniciarPinos :: [String]
iniciarPinos = ["0" | x <- [0..9], True]

integerToString :: Integer -> String
integerToString n = show n

integerToInt :: Integer -> Int
integerToInt n = fromInteger n

pinoFoiDerrubado :: String -> Bool
pinoFoiDerrubado valor = valor == "X"

derrubarPino :: Integer -> [String] -> [String]
derrubarPino posicaoPino pinos = take (integerToInt posicaoPino) pinos ++ "X" : drop ((integerToInt posicaoPino)+1) pinos

resultadoJogada :: Integer -> [String] -> IO [String]
resultadoJogada pinosParaDerrubar pinos = do
  if (pinosParaDerrubar <= 0) then do
    -- print pinos
    return pinos
  else do
    -- print pinos
    pinoParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
    -- putStrLn("Pino Derrubado!")
    -- print pinoParaDerrubar
    resultadoJogada (pinosParaDerrubar-1) (derrubarPino pinoParaDerrubar pinos)

filtraPinosNovosDerrubados :: [String] -> [String] -> [String]
filtraPinosNovosDerrubados posAterior posNova = filter pinoFoiDerrubado (posNova \\ posAterior)

calculaPontos :: [String] -> Int
calculaPontos novosPinosDerrubados = length novosPinosDerrubados

calculaPontuacao :: [String] -> [String] -> Int
calculaPontuacao pinosPosicaoAnterior pinosNovaPosicao = (calculaPontos (filtraPinosNovosDerrubados pinosPosicaoAnterior pinosNovaPosicao))

iniciarLance :: [String] -> Int -> Integer -> IO (Int, Int)
iniciarLance pinos pontuacao nLance = do
  if (nLance == 1) then do
    pinosParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
    print pinosParaDerrubar
    resultadoPinos <- (resultadoJogada pinosParaDerrubar pinos)
    -- print (filtraPinosNovosDerrubados pinos resultadoPinos)
    -- putStrLn("Pontuacao!")
    -- print (calculaPontuacao pinos resultadoPinos)
    return (pontuacao, (calculaPontuacao pinos resultadoPinos))
  else do
      pinosParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
      print pinosParaDerrubar
      if (pinosParaDerrubar == 9) then do return (10,0) -- saberemos que é um Strike
      else do
        resultadoPinos <- (resultadoJogada pinosParaDerrubar pinos)
        -- print (filtraPinosNovosDerrubados pinos resultadoPinos)
        -- putStrLn("Pontuacao!")
        -- print (calculaPontuacao pontuacao pinos resultadoPinos)
        iniciarLance resultadoPinos (calculaPontuacao pinos resultadoPinos) (nLance+1)

somaPontosLance :: (Int, Int) -> Int
somaPontosLance tuplePontuacao = (fst tuplePontuacao) + (snd tuplePontuacao)

-- bonusCasoStrikeSomaDePontos :: Integer -> Integer -> Int
-- bonusCasoStrikeSomaDePontos strikesSeguidos sparesSeguidos = do
--     if (strikesSeguidos > 0) then return (integerToInt strikesSeguidos) * 10
--     else
--       if (sparesSeguidos > 0) then return (integerToInt sparesSeguidos) * 10
--       else return 0
--     return 0

-- somarPontosStrikeSpare :: Integer -> Integer -> Int -> Int
-- somarPontosStrikeSpare strikesSeguidos sparesSeguidos pontuacaoLance = 
--   if (strikesSeguidos > 0) then return (pontuacaoLance)

comecarJogo :: Integer -> Int -> Integer -> Integer -> IO ()
comecarJogo rodada pontuacao strikesSeguidos sparesSeguidos = do
  if (rodada > 10) then do
    putStrLn("Fim de jogo!")
  else do
    putStrLn("Essa é a rodada " ++ (integerToString rodada))
    putStrLn("Digite algo para começar!")
    jogada <- getLine
    putStrLn("Pontuacao antes da jogada")
    print pontuacao
    pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
    if ((fst pontuacaoLance) == 10) then comecarJogo (rodada+1) (pontuacao+10) (strikesSeguidos+1) 0
    else 
      if ((somaPontosLance pontuacaoLance) == 10) then comecarJogo (rodada+1) (pontuacao+10) 0 (sparesSeguidos+1)
      else comecarJogo (rodada+1) (pontuacao+(somaPontosLance pontuacaoLance)) 0 0

-- falta adicionar bonus por strike/spare


main :: IO ()
main = do
  putStrLn "Boliche Arcade"
  putStrLn "Digite seu nome:"
  nome <- getLine
  putStrLn ("Olá " ++ nome ++ ", vamos começar!")
  putStrLn "Iniciando o jogo..."
  comecarJogo 1 0 0 0


