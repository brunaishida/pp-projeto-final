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
      -- imprimeScore (adicionarPontoDaRodada pontosCadaRodada pontuacaoLance rodada) (fst (calcularNovaPontuacao pontuacao (adicionarPontoPendente pontosPendentes pontuacaoLance rodada) 0))
      imprimeTabela(adicionarPontoDaRodada pontosCadaRodada pontuacaoLance rodada) (fst (calcularNovaPontuacao pontuacao (adicionarPontoPendente pontosPendentes pontuacaoLance rodada) 0))
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

-- if pendente then "..."

trocaCaracteres1 :: (Int,Int) -> String
trocaCaracteres1 pontosCadaRodada =
  if ((fst pontosCadaRodada) == -1 || (fst pontosCadaRodada) == 10)
    then " "
    else show (fst pontosCadaRodada)

trocaCaracteres2 :: (Int,Int) -> String
trocaCaracteres2 pontosCadaRodada =
  if ((snd pontosCadaRodada) == -1)
    then " "
    else if ((fst pontosCadaRodada) == 10)
      then "X"
      else if ((somaPontosLance pontosCadaRodada) == 10)
        then "/"
        else show (snd pontosCadaRodada)

trocaCaracteresTotal :: [Int] -> Int -> Int -> (Int, Int) -> String
trocaCaracteresTotal pontuacaoArray (-1) pontTotal pontosCadaRodada = 
  if (pontTotal > 10)
      then (show pontTotal ++ "  ")
      else (show pontTotal ++ "   ")
trocaCaracteresTotal pontuacaoArray pos pontTotal pontosCadaRodada = 
  if ((pontuacaoArray!!pos == 0) && ((somaPontosLance pontosCadaRodada) /= 0))
    then " "
    else (trocaCaracteresTotal pontuacaoArray (pos-1) (pontTotal + (pontuacaoArray!!pos)) (0,0))
    
--fazer trocaCaracteres pro round 10

-- 10,0 - strike
-- 0,10 - spare

imprimeTabela :: [(Int,Int)] -> [Int] -> IO()
imprimeTabela pontosCadaRodada pontuacao = do 
  putStrLn("------------------------------------------------------------------------------------------------")
  putStrLn("|   1   |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   |     10    |")
  putStrLn(pos1 ++ pos2 ++ pos3 ++ pos4 ++ pos5 ++ pos6 ++ pos7 ++ pos8 ++ pos9 ++ pos101 ++ pos102 ++ pos103)
  putStrLn(pont1 ++ pont2 ++ pont3 ++ pont4 ++ pont5 ++ pont6 ++ pont7 ++ pont8 ++ pont9 ++ pont10) --chama funcao pontoscada
  putStrLn("------------------------------------------------------------------------------------------------")
  where
    pos1 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!0)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!0)) ++ " "
    pos2 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!1)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!1)) ++ " "
    pos3 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!2)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!2)) ++ " "
    pos4 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!3)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!3)) ++ " "
    pos5 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!4)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!4)) ++ " "
    pos6 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!5)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!5)) ++ " "
    pos7 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!6)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!6)) ++ " "
    pos8 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!7)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!7)) ++ " "
    pos9 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!8)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!8)) ++ " "
    pos101 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!9)) ++ " " -- necessario arrumar para casos de strike (round extra)
    pos102 = "| " ++ (trocaCaracteres2(pontosCadaRodada!!9)) ++ " "
    pos103 = "| " ++ (trocaCaracteres2(pontosCadaRodada!!9)) ++ " |"
    pont1 = "|   " ++ trocaCaracteresTotal pontuacao 0 0 (pontosCadaRodada!!0)
    pont2 = "|   " ++ trocaCaracteresTotal pontuacao 1 0 (pontosCadaRodada!!1)
    pont3 = "|   " ++ trocaCaracteresTotal pontuacao 2 0 (pontosCadaRodada!!2)
    pont4 = "|   " ++ trocaCaracteresTotal pontuacao 3 0 (pontosCadaRodada!!3)
    pont5 = "|   " ++ trocaCaracteresTotal pontuacao 4 0 (pontosCadaRodada!!4)
    pont6 = "|   " ++ trocaCaracteresTotal pontuacao 5 0 (pontosCadaRodada!!5)
    pont7 = "|   " ++ trocaCaracteresTotal pontuacao 6 0 (pontosCadaRodada!!6)
    pont8 = "|   " ++ trocaCaracteresTotal pontuacao 7 0 (pontosCadaRodada!!7)
    pont9 = "|   " ++ trocaCaracteresTotal pontuacao 8 0 (pontosCadaRodada!!8)
    pont10 = "|     " ++ trocaCaracteresTotal pontuacao 9 0  (pontosCadaRodada!!9) ++ "     |"

-- ideia mental: usar pontosCadaRodada pra cada espaço, sendo pontosCadaRodada!!0 pro 1 (fst pontosCadaRodada!!0 , snd pontosCadaRodada!!0)
-- ai se for = -1/vazio, imprime o espaço, se sair (10,0) aka strike, imprimir fst = " " e snd = "X"
-- semelhante pro spare, usar fst pontosCadaRodada e snd = "/"
-- embaixo eh o pontuacao

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