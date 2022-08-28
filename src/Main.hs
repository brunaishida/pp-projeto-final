module Main where
import System.Random
import Data.List
import Desenho
import Database
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

integerToString :: Integer -> String
integerToString n = show n

integerToInt :: Integer -> Int
integerToInt n = fromInteger n

iniciarPinos :: [String]
iniciarPinos = ["0" | x <- [0..9], True]

iniciarPontosCadaRodada :: [(Int, Int)]
iniciarPontosCadaRodada = [(-1,-1) | x <- [0..11], True]

adicionarPontoDaRodada :: [(Int, Int)] -> (Int, Int) -> Integer -> [(Int, Int)]
adicionarPontoDaRodada pontosDaRodada novoPonto posicao = take (integerToInt posicao) pontosDaRodada ++ novoPonto : drop ((integerToInt posicao)+1) pontosDaRodada

iniciarPontuacao :: [Int]
iniciarPontuacao = [0 | x <- [0..9], True]

somaTotalPotuacao :: [Int] -> Int
somaTotalPotuacao [] = 0
somaTotalPotuacao (x:xs) = x + (somaTotalPotuacao xs)

adicionarPontuacao :: [Int] -> Int -> Integer -> [Int]
adicionarPontuacao pontuacao novoPonto posicao = take (integerToInt posicao) pontuacao ++ ((pontuacao!!(integerToInt posicao))+novoPonto) : drop ((integerToInt posicao)+1) pontuacao
 
iniciarPontosPendentes :: [(Int, Int)]
iniciarPontosPendentes = [(-1,-1) | x <- [0..11], True]

adicionarPontoPendente :: [(Int, Int)] -> (Int, Int) -> Integer -> [(Int, Int)]
adicionarPontoPendente pontosPendentes novoPonto posicao = take (integerToInt posicao) pontosPendentes ++ novoPonto : drop ((integerToInt posicao)+1) pontosPendentes

zerarPontoPendente :: [(Int, Int)] -> Integer -> [(Int, Int)]
zerarPontoPendente pontosPendentes posicao = adicionarPontoPendente pontosPendentes (-1,-1) posicao

pontuacaoPendentePreenchida :: (Int, Int) -> Bool
pontuacaoPendentePreenchida pontuacaoPendente = (fst pontuacaoPendente) > -1

somaPontosLance :: (Int, Int) -> Int
somaPontosLance tuplePontuacao = (fst tuplePontuacao) + (snd tuplePontuacao)

imprimeScore :: [(Int,Int)] -> [Int] -> IO()
imprimeScore pontosCadaRodada pontuacao = do
  putStrLn("scores")

validaResultadoRodada :: (Int,Int) -> String
validaResultadoRodada resultado = if (fst resultado == 10) then "Strike"
  else 
    if (((fst resultado) + (snd resultado)) == 10) then "Spare"
    else "Normal"

calculaPontuacaoStrike :: [(Int,Int)] -> Int -> Integer -> Integer -> Int
calculaPontuacaoStrike pontuacoesPendente pontuacao posicao 0 = pontuacao+10
calculaPontuacaoStrike pontuacoesPendente pontuacao posicao 1 = 
  if (pontuacaoPendentePreenchida (pontuacoesPendente!!((integerToInt posicao)+2))) then
    if (validaResultadoRodada (pontuacoesPendente!!((integerToInt posicao)+2)) == "Strike") then
      calculaPontuacaoStrike pontuacoesPendente (pontuacao+10) posicao 0
    else calculaPontuacaoStrike pontuacoesPendente (pontuacao + fst (pontuacoesPendente!!((integerToInt posicao)+1))) posicao 0
  else 0
calculaPontuacaoStrike pontuacoesPendente pontuacao posicao 2 =
  if (pontuacaoPendentePreenchida (pontuacoesPendente!!((integerToInt posicao)+1))) then
    if (validaResultadoRodada (pontuacoesPendente!!((integerToInt posicao)+1)) == "Strike") then
      calculaPontuacaoStrike pontuacoesPendente (pontuacao+10) posicao 1
    else calculaPontuacaoStrike pontuacoesPendente (pontuacao+somaPontosLance (pontuacoesPendente!!((integerToInt posicao)+1))) posicao 0
  else 0

calculaPontuacaoSpare :: [(Int, Int)] -> Int -> Integer -> Integer -> Int
calculaPontuacaoSpare pontuacoesPendente pontuacao posicao 0 = pontuacao+10
calculaPontuacaoSpare pontuacoesPendente pontuacao posicao 1 = 
  if (pontuacaoPendentePreenchida (pontuacoesPendente!!((integerToInt posicao)+1))) then
    calculaPontuacaoStrike pontuacoesPendente (pontuacao+ fst (pontuacoesPendente!!((integerToInt posicao)+1))) posicao 0
  else 0

calcularNovaPontuacao :: [Int] -> [(Int,Int)] -> Integer -> ([Int], [(Int,Int)])
calcularNovaPontuacao pontuacao pontuacoesPendente 9 = do
  if ((fst(pontuacoesPendente!!11)) > -1) then 
    (
      adicionarPontuacao pontuacao (
        (somaPontosLance (pontuacoesPendente!!9)) + (somaPontosLance (pontuacoesPendente!!10)) + (somaPontosLance (pontuacoesPendente!!11))
      ) 9, 
      (zerarPontoPendente (zerarPontoPendente (zerarPontoPendente pontuacoesPendente 9) 10) 11)
    )
  else 
    if ((fst(pontuacoesPendente!!10)) > -1) then 
      (
        adicionarPontuacao pontuacao (
          (somaPontosLance (pontuacoesPendente!!9)) + (somaPontosLance (pontuacoesPendente!!10))
        ) 9, 
        (zerarPontoPendente (zerarPontoPendente pontuacoesPendente 9) 10)
      )
    else 
      if ((fst(pontuacoesPendente!!9)) > -1) then 
      (
        adicionarPontuacao pontuacao (
          (somaPontosLance (pontuacoesPendente!!9))
        ) 9, 
        (zerarPontoPendente pontuacoesPendente 9)
      )
      else
        (pontuacao, pontuacoesPendente)
calcularNovaPontuacao pontuacao pontuacoesPendente posicao = do
  if (pontuacaoPendentePreenchida (pontuacoesPendente!!(integerToInt posicao))) then
    if (validaResultadoRodada (pontuacoesPendente!!(integerToInt posicao)) == "Strike") then
      if ((calculaPontuacaoStrike pontuacoesPendente 0 posicao 2) > 0) then
        calcularNovaPontuacao 
          (adicionarPontuacao pontuacao (calculaPontuacaoStrike pontuacoesPendente 0 posicao 2) posicao) 
          (zerarPontoPendente pontuacoesPendente posicao) 
          (posicao+1)
      else calcularNovaPontuacao pontuacao pontuacoesPendente (posicao+1)
    else 
      if (validaResultadoRodada (pontuacoesPendente!!(integerToInt posicao)) == "Spare") then
        if ((calculaPontuacaoSpare pontuacoesPendente 0 posicao 1) > 0) then
          calcularNovaPontuacao
            (adicionarPontuacao pontuacao (calculaPontuacaoSpare pontuacoesPendente 0 posicao 1) posicao)
            (zerarPontoPendente pontuacoesPendente posicao)
            (posicao+1)
        else calcularNovaPontuacao pontuacao pontuacoesPendente (posicao+1)
      else calcularNovaPontuacao 
        (adicionarPontuacao pontuacao (somaPontosLance (pontuacoesPendente!!(integerToInt posicao))) posicao) 
        (zerarPontoPendente pontuacoesPendente posicao) 
        (posicao+1)
  else calcularNovaPontuacao pontuacao pontuacoesPendente (posicao+1)

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
    -- print pinosParaDerrubar
    resultadoPinos <- (resultadoJogada pinosParaDerrubar pinos)
    -- print (filtraPinosNovosDerrubados pinos resultadoPinos)
    -- putStrLn("Pontuacao!")
    -- print (calculaPontuacao pinos resultadoPinos)
    return (pontuacao, (calculaPontuacao pinos resultadoPinos))
  else do
      pinosParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
      if (pinosParaDerrubar == 9) then do return (10,0) -- saberemos que é um Strike
      else do
        resultadoPinos <- (resultadoJogada pinosParaDerrubar pinos)
        -- print (filtraPinosNovosDerrubados pinos resultadoPinos)
        -- putStrLn("Pinos Derrubados!")
        -- print (calculaPontuacao pinos resultadoPinos)
        iniciarLance resultadoPinos (calculaPontuacao pinos resultadoPinos) (nLance+1)

casoRodada10 :: [(Int, Int)] -> Integer -> IO [(Int, Int)] 
casoRodada10 pontosPendentes 0 = do
  putStrLn("Rodada 0")
  print pontosPendentes
  pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
  return (adicionarPontoPendente pontosPendentes ((fst pontuacaoLance),0) 11)
casoRodada10 pontosPendentes 1 = do 
  putStrLn("Rodada 1")
  print pontosPendentes
  pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
  if (somaPontosLance pontuacaoLance == 10) then 
     casoRodada10 (adicionarPontoPendente pontosPendentes (10,0) 10) 0
  else
      return (adicionarPontoPendente pontosPendentes pontuacaoLance 10)
casoRodada10 pontosPendentes 2 = do
  putStrLn("Rodada 2")
  print pontosPendentes
  pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
  if (fst pontuacaoLance == 10) then 
    casoRodada10 (adicionarPontoPendente pontosPendentes (10,0) 9) 1
  else
    if (somaPontosLance pontuacaoLance == 10) then
      casoRodada10 (adicionarPontoPendente pontosPendentes pontuacaoLance 9) 0
    else
      return (adicionarPontoPendente pontosPendentes pontuacaoLance 9)

comecarJogo :: Integer -> [Int] -> [(Int, Int)] -> [(Int, Int)] -> IO ()
comecarJogo rodada pontuacao pontosCadaRodada pontosPendentes = do
  if (rodada > 9) then do
    putStrLn("Fim de jogo!")
  else do
    putStrLn("Essa é a rodada " ++ (integerToString rodada))
    putStrLn("Digite algo para começar!")
    jogada <- getLine
    putStrLn("Pontuacao antes da jogada")
    print pontuacao
    if (rodada == 9) then do
      resultadoPontosPendentes <- casoRodada10 pontosPendentes 2
      print resultadoPontosPendentes
      print (calcularNovaPontuacao pontuacao resultadoPontosPendentes 0)
      putStrLn("Chegou aqui!")
      comecarJogo (rodada+1) (fst (calcularNovaPontuacao pontuacao resultadoPontosPendentes 0)) pontosCadaRodada (snd (calcularNovaPontuacao pontuacao resultadoPontosPendentes 0))
    else do 
      pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
      putStrLn("Pinos derrubados")
      print pontuacaoLance
      print pontosCadaRodada
      print (calcularNovaPontuacao pontuacao (adicionarPontoPendente pontosPendentes pontuacaoLance rodada) 0)
      imprimeScore (adicionarPontoDaRodada pontosCadaRodada pontuacaoLance rodada) (fst (calcularNovaPontuacao pontuacao (adicionarPontoPendente pontosPendentes pontuacaoLance rodada) 0))
      comecarJogo 
        (rodada+1) 
        (fst (calcularNovaPontuacao pontuacao (adicionarPontoPendente pontosPendentes pontuacaoLance rodada) 0))
        (adicionarPontoDaRodada pontosCadaRodada pontuacaoLance rodada)
        (snd (calcularNovaPontuacao pontuacao (adicionarPontoPendente pontosPendentes pontuacaoLance rodada) 0))

-- handleEvent :: Event -> Frames -> Frames
-- handleEvent (EventKey (SpecialKey k) Down _ _) =
--   if rodada > 9 && mundoUm                                       
--      then fimJogoUm                                         
--      else jogada

-- imprimir os scores
-- multiplayer

main :: IO ()
main = do
  putStrLn "Boliche Arcade"
  putStrLn "Digite seu nome:"
  nome <- getLine
  putStrLn ("Olá " ++ nome ++ ", vamos começar!")
  putStrLn ("Quantas pessoas vão jogar? Contando com você. Digite um número de 1-4.")
  jogadores <- getLine
  putStrLn "Iniciando o jogo..."
  comecarJogo 0 iniciarPontuacao iniciarPontosCadaRodada iniciarPontosPendentes

  -- teste grafico
    -- display 
    --   janela
    --   white
    --   mundoUm
    --   where
    --     janela = (InWindow "Boliche Arcade" (1200, 680) (50, 50)) 