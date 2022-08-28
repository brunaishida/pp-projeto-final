module Main where
import System.Random
import Data.List

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
    putStrLn("*-------------------------Rodada " ++ (integerToString (rodada+1)) ++ "-------------------------*")
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

iniciarRodadaParaCadaJogador :: Integer -> Int -> [String] -> [[Int]] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> IO([[Int]], [[(Int, Int)]], [[(Int, Int)]])
iniciarRodadaParaCadaJogador rodada numJogadores nomesJogadores pontuacao pontosCadaRodada pontosPendentes counter = do
  if (counter == numJogadores) then do
    return (pontuacao, pontosCadaRodada, pontosPendentes)
  else do
    putStrLn("Agora é a vez do "++ (nomesJogadores!!counter))
    pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
    putStrLn("Pinos derrubados")
    let pontuacaoDoJogador = pontuacao!!counter
    -- print pontuacaoDoJogador
    let pontosPendentesDoJogador = pontosPendentes!!counter
    -- print pontosPendentesDoJogador
    let pontosCadaRodadaDoJogador = pontosCadaRodada!!counter
    -- print pontosCadaRodadaDoJogador
    let (novaPontuacaoDoJogador, novosPontosPendentesDoJogador) = (calcularNovaPontuacao pontuacaoDoJogador (adicionarPontoPendente pontosPendentesDoJogador pontuacaoLance rodada) 0)
    print novaPontuacaoDoJogador
    print novosPontosPendentesDoJogador
    let novosPontosCadaRodadaDoJogador = (adicionarPontoDaRodada pontosCadaRodadaDoJogador pontuacaoLance rodada)
    print novosPontosCadaRodadaDoJogador
    let novaPontuacaoJogadores = adicionarPontuacaoJogadores pontuacao novaPontuacaoDoJogador counter
    let novaPontuacaoPendenteJogadores = adicionarPontosPendentesJogadores pontosPendentes novosPontosPendentesDoJogador counter
    let novaPontuacaoCadaRodadaJogadores = adicionarPontoCadaRodadaJogadores pontosCadaRodada novosPontosCadaRodadaDoJogador counter
    iniciarRodadaParaCadaJogador rodada numJogadores nomesJogadores novaPontuacaoJogadores novaPontuacaoCadaRodadaJogadores novaPontuacaoPendenteJogadores (counter+1)

comecarJogoMultiplayer :: Integer -> Int -> [String] -> [[Int]] -> [[(Int, Int)]] -> [[(Int, Int)]] -> IO()
comecarJogoMultiplayer rodada numJogadores nomesJogadores pontuacao pontosCadaRodada pontosPendentes = do
  if (rodada > 9) then do
    putStrLn("Fim de jogo!")
  else do
    putStrLn("*-------------------------Rodada " ++ (integerToString (rodada+1)) ++ "-------------------------*")
    resultadoRodada <- (iniciarRodadaParaCadaJogador rodada numJogadores nomesJogadores pontuacao pontosCadaRodada pontosPendentes 0)
    let (novaPontuacao, novosPontosCadaRodada, novosPontosPendentes) = resultadoRodada
    comecarJogoMultiplayer (rodada+1) numJogadores nomesJogadores novaPontuacao novosPontosCadaRodada novosPontosPendentes

-- imprimir os scores

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
  print nomesJogadores
  putStrLn "Iniciando o jogo..."
  let pontuacao = iniciarPontuacaoMultiplayer nJogadoresInt
  let pontosCadaRodada = iniciarPontosCadaRodadaMultiplayer nJogadoresInt
  let pontosPendentes = iniciarPontosPendentesMultiplayer nJogadoresInt
  print pontuacao
  print pontosCadaRodada
  print pontosPendentes
  comecarJogoMultiplayer 0 nJogadoresInt nomesJogadores pontuacao pontosCadaRodada pontosPendentes
  -- comecarJogo 0 iniciarPontuacao iniciarPontosCadaRodada iniciarPontosPendentes


