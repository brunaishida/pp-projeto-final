module Main where
import System.Random
import Data.List
import Data.Function (on)
import Data.Ord
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Desenho
import Database

intToString :: Int -> String
intToString n = show n

stringToInt :: String -> Int
stringToInt n = read n ::Int

integerToString :: Integer -> String
integerToString n = show n

integerToInt :: Integer -> Int
integerToInt n = fromInteger n

iniciarPinos :: [String]
iniciarPinos = ["0" | x <- [0..9], True]

iniciarPontosCadaRodada :: [(Int, Int)]
iniciarPontosCadaRodada = [(-1,-1) | x <- [0..11], True]

iniciarPontosCadaRodadaMultiplayer :: Int -> [[(Int, Int)]]
iniciarPontosCadaRodadaMultiplayer n = [iniciarPontosCadaRodada | x <- [0..(n-1)], True]

adicionarPontoDaRodada :: [(Int, Int)] -> (Int, Int) -> Integer -> [(Int, Int)]
adicionarPontoDaRodada pontosDaRodada novoPonto posicao = take (integerToInt posicao) pontosDaRodada ++ novoPonto : drop ((integerToInt posicao)+1) pontosDaRodada

adicionarPontoCadaRodadaJogadores :: [[(Int, Int)]] -> [(Int, Int)] -> Int -> [[(Int, Int)]]
adicionarPontoCadaRodadaJogadores pontosCadaRodadaJogadores novoPonto posicao = take posicao pontosCadaRodadaJogadores ++ novoPonto : drop (posicao+1) pontosCadaRodadaJogadores

iniciarPontuacao :: [Int]
iniciarPontuacao = [0 | x <- [0..9], True]

iniciarPontuacaoMultiplayer :: Int -> [[Int]]
iniciarPontuacaoMultiplayer n = [iniciarPontuacao | x <- [0..(n-1)], True]

somaTotalPotuacao :: [Int] -> Int
somaTotalPotuacao [] = 0
somaTotalPotuacao (x:xs) = x + (somaTotalPotuacao xs)

adicionarPontuacao :: [Int] -> Int -> Integer -> [Int]
adicionarPontuacao pontuacao novoPonto posicao = take (integerToInt posicao) pontuacao ++ ((pontuacao!!(integerToInt posicao))+novoPonto) : drop ((integerToInt posicao)+1) pontuacao
 
adicionarPontuacaoJogadores :: [[Int]] -> [Int] -> Int -> [[Int]]
adicionarPontuacaoJogadores pontuacaoJogadores novaPontuacao posicao = take posicao pontuacaoJogadores ++ novaPontuacao : drop (posicao+1) pontuacaoJogadores

iniciarPontosPendentes :: [(Int, Int)]
iniciarPontosPendentes = [(-1,-1) | x <- [0..11], True]

iniciarPontosPendentesMultiplayer :: Int -> [[(Int, Int)]]
iniciarPontosPendentesMultiplayer n = [iniciarPontosPendentes | x <- [0..(n-1)], True]

adicionarPontoPendente :: [(Int, Int)] -> (Int, Int) -> Integer -> [(Int, Int)]
adicionarPontoPendente pontosPendentes novoPonto posicao = take (integerToInt posicao) pontosPendentes ++ novoPonto : drop ((integerToInt posicao)+1) pontosPendentes

adicionarPontosPendentesJogadores :: [[(Int, Int)]] -> [(Int, Int)] -> Int -> [[(Int, Int)]]
adicionarPontosPendentesJogadores pontosPendentesJogadores novoPontosPendentes posicao = 
  take posicao pontosPendentesJogadores ++ novoPontosPendentes : drop (posicao+1) pontosPendentesJogadores

zerarPontoPendente :: [(Int, Int)] -> Integer -> [(Int, Int)]
zerarPontoPendente pontosPendentes posicao = adicionarPontoPendente pontosPendentes (-1,-1) posicao

pontuacaoPendentePreenchida :: (Int, Int) -> Bool
pontuacaoPendentePreenchida pontuacaoPendente = (fst pontuacaoPendente) > -1

somaPontosLance :: (Int, Int) -> Int
somaPontosLance tuplePontuacao = (fst tuplePontuacao) + (snd tuplePontuacao)

-- imprimeScore :: [(Int,Int)] -> [Int] -> IO()
-- imprimeScore pontosCadaRodada pontuacao = do
--   putStrLn("scores")

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
    else calculaPontuacaoStrike pontuacoesPendente (pontuacao + fst (pontuacoesPendente!!((integerToInt posicao)+2))) posicao 0
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
    putStrLn(" ")
    putStrLn("Lance número 2")
    putStrLn("Aperte enter para jogar a bola!")
    jogada <- getLine
    pinosParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
    resultadoPinos <- (resultadoJogada pinosParaDerrubar pinos)
    let totalPinosDerrubados = (calculaPontuacao pinos resultadoPinos)
    putStrLn("Você acertou "++ (intToString totalPinosDerrubados) ++ " pinos!!")
    putStrLn("---------------Total Lance = " ++ (intToString (pontuacao + totalPinosDerrubados)) ++ " pontos ---------------")
    return (pontuacao, totalPinosDerrubados)
  else do
      putStrLn(" ")
      putStrLn("Lance número 1")
      putStrLn("Aperte enter para jogar a bola!")
      jogada <- getLine
      pinosParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
      if (pinosParaDerrubar == 9) then do 
        putStrLn("Strike!!!!!!!")
        putStrLn("Você acertou 10 pinos!!")
        return (10,0) -- saberemos que é um Strike
      else do
        resultadoPinos <- (resultadoJogada pinosParaDerrubar pinos)
        let totalPinosDerrubados = (calculaPontuacao pinos resultadoPinos)
        putStrLn("Você acertou "++ (intToString totalPinosDerrubados) ++ " pinos!!")
        iniciarLance resultadoPinos totalPinosDerrubados (nLance+1)

casoRodada10 :: [(Int, Int)] -> [(Int, Int)] -> Integer -> IO ([(Int, Int)], [(Int, Int)]) 
casoRodada10 pontosPendentes pontosCadaRodada 0 = do
  -- putStrLn("Rodada 0")
  -- print pontosPendentes
  putStrLn("Aperte enter para jogar a bola!")
  jogada <- getLine
  pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
  let primeiroLance = fst pontuacaoLance
  let valorRodada = (primeiroLance, 0)
  let novosPontosPendentes = (adicionarPontoPendente pontosPendentes valorRodada 11)
  let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada valorRodada 11)
  return (novosPontosPendentes, novosPontosCadaRodada)
casoRodada10 pontosPendentes pontosCadaRodada 1 = do 
  -- putStrLn("Rodada 1")
  -- print pontosPendentes
  putStrLn("Aperte enter para jogar a bola!")
  jogada <- getLine
  pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
  if (somaPontosLance pontuacaoLance == 10) then do
      let valorRodada = (10, 0)
      let novosPontosPendentes = (adicionarPontoPendente pontosPendentes valorRodada 10)
      let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada valorRodada 10)
      casoRodada10 novosPontosPendentes novosPontosCadaRodada 0
  else do
      let novosPontosPendentes = (adicionarPontoPendente pontosPendentes pontuacaoLance 10)
      let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada pontuacaoLance 10)
      return (novosPontosPendentes, novosPontosCadaRodada)
casoRodada10 pontosPendentes pontosCadaRodada 2 = do
  -- putStrLn("Rodada 2")
  -- print pontosPendentes
  putStrLn("Aperte enter para jogar a bola!")
  jogada <- getLine
  pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
  if (fst pontuacaoLance == 10) then do
    let valorRodada = (10, 0)
    let novosPontosPendentes = (adicionarPontoPendente pontosPendentes valorRodada 9)
    let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada valorRodada 9)
    casoRodada10 novosPontosPendentes novosPontosCadaRodada 1
  else do
    if (somaPontosLance pontuacaoLance == 10) then do
      let novosPontosPendentes = (adicionarPontoPendente pontosPendentes pontuacaoLance 9)
      let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada pontuacaoLance 9)
      casoRodada10 novosPontosPendentes novosPontosCadaRodada 0
    else do
      let novosPontosPendentes = (adicionarPontoPendente pontosPendentes pontuacaoLance 9)
      let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada pontuacaoLance 9)
      return (novosPontosPendentes, novosPontosCadaRodada)

iniciarRodadaParaCadaJogador :: Integer -> Int -> [String] -> [[Int]] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> IO([[Int]], [[(Int, Int)]], [[(Int, Int)]])
iniciarRodadaParaCadaJogador rodada numJogadores nomesJogadores pontuacao pontosCadaRodada pontosPendentes counter = do
  if (counter == numJogadores) then do
    return (pontuacao, pontosCadaRodada, pontosPendentes)
  else do
    putStrLn(" ")
    putStrLn("*** Agora é a vez de "++ (nomesJogadores!!counter) ++ " ***")
    pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
    -- putStrLn("Pinos derrubados")
    let pontuacaoDoJogador = pontuacao!!counter
    -- print pontuacaoDoJogador
    let pontosPendentesDoJogador = pontosPendentes!!counter
    -- print pontosPendentesDoJogador
    let pontosCadaRodadaDoJogador = pontosCadaRodada!!counter
    -- print pontosCadaRodadaDoJogador
    let nomeJogador = nomesJogadores!!counter
    let (novaPontuacaoDoJogador, novosPontosPendentesDoJogador) = (calcularNovaPontuacao pontuacaoDoJogador (adicionarPontoPendente pontosPendentesDoJogador pontuacaoLance rodada) 0)
    -- print novaPontuacaoDoJogador
    -- print novosPontosPendentesDoJogador
    let novosPontosCadaRodadaDoJogador = (adicionarPontoDaRodada pontosCadaRodadaDoJogador pontuacaoLance rodada)
    imprimeTabela nomeJogador novosPontosCadaRodadaDoJogador novaPontuacaoDoJogador
    -- print novosPontosCadaRodadaDoJogador
    let novaPontuacaoJogadores = adicionarPontuacaoJogadores pontuacao novaPontuacaoDoJogador counter
    let novaPontuacaoPendenteJogadores = adicionarPontosPendentesJogadores pontosPendentes novosPontosPendentesDoJogador counter
    let novaPontuacaoCadaRodadaJogadores = adicionarPontoCadaRodadaJogadores pontosCadaRodada novosPontosCadaRodadaDoJogador counter
    iniciarRodadaParaCadaJogador rodada numJogadores nomesJogadores novaPontuacaoJogadores novaPontuacaoCadaRodadaJogadores novaPontuacaoPendenteJogadores (counter+1)

iniciarRodada10ParaCadaJogador :: Integer -> Int -> [String] -> [[Int]] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> IO([[Int]], [[(Int, Int)]], [[(Int, Int)]])
iniciarRodada10ParaCadaJogador rodada numJogadores nomesJogadores pontuacao pontosCadaRodada pontosPendentes counter = do
  if (counter == numJogadores) then do
    return (pontuacao, pontosCadaRodada, pontosPendentes)
  else do
    putStrLn(" ")
    putStrLn("*** Agora é a vez de "++ (nomesJogadores!!counter) ++ " ***")
    let nomeJogador = nomesJogadores!!counter
    let pontuacaoDoJogador = pontuacao!!counter
    -- print pontuacaoDoJogador
    let pontosPendentesDoJogador = pontosPendentes!!counter
    -- print pontosPendentesDoJogador
    let pontosCadaRodadaDoJogador = pontosCadaRodada!!counter
    -- print pontosCadaRodadaDoJogador
    resultadoPontosPendentes <- casoRodada10 pontosPendentesDoJogador pontosCadaRodadaDoJogador 2
    let (novosPontosPendentesDoJogador1, novosPontosCadaRodadaDoJogador) = resultadoPontosPendentes
    let (novaPontuacaoDoJogador, novosPontosPendentesDoJogador) = (calcularNovaPontuacao pontuacaoDoJogador novosPontosPendentesDoJogador1 0)
    let novaPontuacaoJogadores = adicionarPontuacaoJogadores pontuacao novaPontuacaoDoJogador counter
    let novaPontuacaoPendenteJogadores = adicionarPontosPendentesJogadores pontosPendentes novosPontosPendentesDoJogador counter
    let novaPontuacaoCadaRodadaJogadores = adicionarPontoCadaRodadaJogadores pontosCadaRodada novosPontosCadaRodadaDoJogador counter
    imprimeTabela nomeJogador novosPontosCadaRodadaDoJogador novaPontuacaoDoJogador
    -- print novaPontuacaoJogadores
    -- print novaPontuacaoPendenteJogadores
    -- print novaPontuacaoCadaRodadaJogadores
    iniciarRodada10ParaCadaJogador rodada numJogadores nomesJogadores novaPontuacaoJogadores novaPontuacaoCadaRodadaJogadores novaPontuacaoPendenteJogadores (counter+1)


pontuacaoTotal :: [[Int]] -> [Int]
pontuacaoTotal [] = []
pontuacaoTotal (x:xs) = somaTotalPotuacao x : pontuacaoTotal xs

sortGT :: (Int, String) -> (Int, String) -> Ordering
sortGT (a1, b1) (a2, b2)
  | a1 < a2 = GT
  | a1 > a2 = LT
  | a1 == a2 = compare b1 b2

ordenaPeloPrimeiroDaTupla :: [(Int, String)] -> [(Int, String)]
ordenaPeloPrimeiroDaTupla = sortBy sortGT

transformaEmTuplaDeRanking :: Int -> [String] -> [Int] -> [(Int, String)]
transformaEmTuplaDeRanking numJogadores nomesJogadores pontuacaoTotal = 
  [((pontuacaoTotal!!x),(nomesJogadores!!x)) | x <- [0..(numJogadores-1)], True]

calculaRanking :: Int -> [String] -> [[Int]] -> IO()
calculaRanking numJogadores nomesJogadores pontuacao = do
  let pontuacaoTotalJogadores = pontuacaoTotal pontuacao
  let tuplaRanking = transformaEmTuplaDeRanking numJogadores nomesJogadores pontuacaoTotalJogadores
  let rankingOrdenado = ordenaPeloPrimeiroDaTupla tuplaRanking
  -- print rankingOrdenado
  let (pontosPrimeiroColocado, nomePrimeiroColocado) = rankingOrdenado!!0
  putStrLn("E o vencedor foi: " ++ nomePrimeiroColocado ++ " com " ++ (intToString pontosPrimeiroColocado) ++ " pontos!")
  let (pontosSegundoColocado, nomeSegundoColocado) = rankingOrdenado!!1
  putStrLn("O segundo colocado foi: " ++ nomeSegundoColocado ++ " com " ++ (intToString pontosSegundoColocado) ++ " pontos!")
  if (numJogadores > 2) then do
    let (pontosTerceiroColocado, nomeTerceiroColocado) = rankingOrdenado!!2
    putStrLn("O terceito colocado foi: " ++ nomeTerceiroColocado ++ " com " ++ (intToString pontosTerceiroColocado) ++ " pontos!")
    if (numJogadores > 3) then do
      let (pontosQuartoColocado, nomeQuartoColocado) = rankingOrdenado!!3
      putStrLn("O quarto colocado foi: " ++ nomeQuartoColocado ++ " com " ++ (intToString pontosQuartoColocado) ++ " pontos!")
      putStrLn("Fim de jogo!")
    else
      putStrLn("Fim de jogo!")
  else
    putStrLn("Fim de jogo!")


comecarJogoMultiplayer :: Integer -> Int -> [String] -> [[Int]] -> [[(Int, Int)]] -> [[(Int, Int)]] -> IO()
comecarJogoMultiplayer rodada numJogadores nomesJogadores pontuacao pontosCadaRodada pontosPendentes = do
  if (rodada > 9) then do
    putStrLn("Fim de jogo!")
    putStrLn("Calculando resultados...")
    calculaRanking numJogadores nomesJogadores pontuacao
  else do
    if (rodada < 9) then do
      putStrLn("*-------------------------Rodada " ++ (integerToString (rodada+1)) ++ "-------------------------*")
      resultadoRodada <- (iniciarRodadaParaCadaJogador rodada numJogadores nomesJogadores pontuacao pontosCadaRodada pontosPendentes 0)
      let (novaPontuacao, novosPontosCadaRodada, novosPontosPendentes) = resultadoRodada
      comecarJogoMultiplayer (rodada+1) numJogadores nomesJogadores novaPontuacao novosPontosCadaRodada novosPontosPendentes
    else do
      putStrLn("*-------------------------Rodada 10-------------------------*")
      resultadoRodada <- (iniciarRodada10ParaCadaJogador rodada numJogadores nomesJogadores pontuacao pontosCadaRodada pontosPendentes 0)
      let (novaPontuacao, novosPontosCadaRodada, novosPontosPendentes) = resultadoRodada
      comecarJogoMultiplayer (rodada+1) numJogadores nomesJogadores novaPontuacao novosPontosCadaRodada novosPontosPendentes


-- tratar o numero de jogadores

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
  if (pontTotal >= 10 && pontTotal < 100)
      then (show pontTotal ++ "  ")
      else if (pontTotal >= 100)
        then (show pontTotal ++ " ")
        else (show pontTotal ++ "   ")
trocaCaracteresTotal pontuacaoArray pos pontTotal pontosCadaRodada = 
  if ((pontuacaoArray!!pos == 0) && ((somaPontosLance pontosCadaRodada) /= 0))
    then "    "
    else (trocaCaracteresTotal pontuacaoArray (pos-1) (pontTotal + (pontuacaoArray!!pos)) (0,0))

trocaCaracteres10 :: (Int,Int) -> String
trocaCaracteres10 pontosCadaRodada = 
   if ((fst pontosCadaRodada) == -1)
    then " "
    else if ((fst pontosCadaRodada) == 10)
      then "X"
      else show (fst pontosCadaRodada)

-- verificaRodada10 :: 
   
--fazer trocaCaracteres pro round 10

-- 10,0 - strike
-- 0,10 - spare

imprimeTabela :: String -> [(Int,Int)] -> [Int] -> IO()
imprimeTabela nomeJogador pontosCadaRodada pontuacao = do 
  putStrLn(" ")
  putStrLn("-------------------------------" ++ nomeJogador ++ "-------------------------------")
  putStrLn("--------------------------------------------------------------------------------------")
  putStrLn("|   1   |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   |     10    |")
  putStrLn(pos1 ++ pos2 ++ pos3 ++ pos4 ++ pos5 ++ pos6 ++ pos7 ++ pos8 ++ pos9 ++ verificaPos10)
  putStrLn(pont1 ++ pont2 ++ pont3 ++ pont4 ++ pont5 ++ pont6 ++ pont7 ++ pont8 ++ pont9 ++ pont10)
  putStrLn("--------------------------------------------------------------------------------------")
  putStrLn(" ")
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
    verificaPos10 = 
      if ((fst (pontosCadaRodada!!9) == 10) && (fst (pontosCadaRodada!!10) /= 10)) -- primeiro lance com strike e outros spare ou nao
        then "| " ++ (trocaCaracteres10(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres1(pontosCadaRodada!!10)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!10)) ++ " |"
        else if ((fst (pontosCadaRodada!!9) == 10) && (fst (pontosCadaRodada!!10) == 10)) -- dois strikes e o ultimo sem strike ou tres strikes
          then "| " ++ (trocaCaracteres10(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres10(pontosCadaRodada!!10)) ++ " " ++ "| " ++ (trocaCaracteres10(pontosCadaRodada!!11)) ++ " |"
          else if ((fst (pontosCadaRodada!!9) /= 10) && ((somaPontosLance (pontosCadaRodada!!9)) == 10) ) -- primeiro lance spare e outro nao strike ou strike
            then "| " ++ (trocaCaracteres1(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres10(pontosCadaRodada!!10)) ++ " |"
            else "| " ++ (trocaCaracteres1(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres1(pontosCadaRodada!!10)) ++ " |"
    pont1 = "|   " ++ trocaCaracteresTotal pontuacao 0 0 (pontosCadaRodada!!0)
    pont2 = "|   " ++ trocaCaracteresTotal pontuacao 1 0 (pontosCadaRodada!!1)
    pont3 = "|   " ++ trocaCaracteresTotal pontuacao 2 0 (pontosCadaRodada!!2)
    pont4 = "|   " ++ trocaCaracteresTotal pontuacao 3 0 (pontosCadaRodada!!3)
    pont5 = "|   " ++ trocaCaracteresTotal pontuacao 4 0 (pontosCadaRodada!!4)
    pont6 = "|   " ++ trocaCaracteresTotal pontuacao 5 0 (pontosCadaRodada!!5)
    pont7 = "|   " ++ trocaCaracteresTotal pontuacao 6 0 (pontosCadaRodada!!6)
    pont8 = "|   " ++ trocaCaracteresTotal pontuacao 7 0 (pontosCadaRodada!!7)
    pont9 = "|   " ++ trocaCaracteresTotal pontuacao 8 0 (pontosCadaRodada!!8)
    pont10 = "|     " ++ trocaCaracteresTotal pontuacao 9 0  (pontosCadaRodada!!9) ++ "  |"
    


adicionaNomeJogador :: [String] -> String -> [String] 
adicionaNomeJogador [] nome = [nome]
adicionaNomeJogador (nomeJogador:nomesJogadores) nome = nomeJogador : (adicionaNomeJogador nomesJogadores nome)

receberNomeJogadores :: Int -> [String] -> Int -> IO [String]
receberNomeJogadores nJogadores nomesJogadores 1 =do return nomesJogadores
receberNomeJogadores nJogadores nomesJogadores counter= do
  putStrLn("Adicione o nome do jogador " ++ (intToString (nJogadores-counter+2)))
  nomeJogador <- getLine
  receberNomeJogadores nJogadores (adicionaNomeJogador nomesJogadores nomeJogador) (counter-1)

main :: IO ()
main = do
  putStrLn "Boliche Arcade"
  putStrLn "Digite seu nome:"
  nome <- getLine
  putStrLn ("Olá " ++ nome ++ ", vamos começar!")
  putStrLn ("Quantas pessoas vão jogar? Contando com você. Digite um número de 2-4.")
  nJogadores <- getLine
  let nJogadoresInt = (stringToInt nJogadores)
  nomesJogadores <- receberNomeJogadores nJogadoresInt [nome] nJogadoresInt
  -- print nomesJogadores -- Verificar se deixa
  putStrLn "Iniciando o jogo..."
  let pontuacao = iniciarPontuacaoMultiplayer nJogadoresInt
  let pontosCadaRodada = iniciarPontosCadaRodadaMultiplayer nJogadoresInt
  let pontosPendentes = iniciarPontosPendentesMultiplayer nJogadoresInt
  -- print pontuacao
  -- print pontosCadaRodada
  -- print pontosPendentes
  comecarJogoMultiplayer 0 nJogadoresInt nomesJogadores pontuacao pontosCadaRodada pontosPendentes
  -- comecarJogo 0 iniciarPontuacao iniciarPontosCadaRodada iniciarPontosPendentes

-- teste grafico
    -- display 
    --   janela
    --   white
    --   mundoUm
    --   where
    --     janela = (InWindow "Boliche Arcade" (1200, 680) (50, 50)) 
