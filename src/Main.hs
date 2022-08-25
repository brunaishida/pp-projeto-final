module Main where
import System.Random
import Data.List

integerToString :: Integer -> String
integerToString n = show n

integerToInt :: Integer -> Int
integerToInt n = fromInteger n

iniciarPinos :: [String]
iniciarPinos = ["0" | x <- [0..9], True]

iniciarPontuacao :: [Int]
iniciarPontuacao = [0 | x <- [0..11], True]

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
calcularNovaPontuacao pontuacao pontuacoesPendente 12 = (pontuacao, pontuacoesPendente)
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

comecarJogo :: Integer -> [Int] -> [(Int, Int)]-> IO ()
comecarJogo rodada pontuacao pontosPendentes = do
  if (rodada > 10) then do
    putStrLn("Fim de jogo!")
  else do
    putStrLn("Essa é a rodada " ++ (integerToString rodada))
    putStrLn("Digite algo para começar!")
    jogada <- getLine
    putStrLn("Pontuacao antes da jogada")
    print pontuacao
    pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
    putStrLn("Pinos derrubados")
    print pontuacaoLance
    print (calcularNovaPontuacao pontuacao (adicionarPontoPendente pontosPendentes pontuacaoLance rodada) 0)
    comecarJogo 
      (rodada+1) 
      (fst (calcularNovaPontuacao pontuacao (adicionarPontoPendente pontosPendentes pontuacaoLance rodada) 0))
      (snd (calcularNovaPontuacao pontuacao (adicionarPontoPendente pontosPendentes pontuacaoLance rodada) 0))


-- falta adicionar bonus 10 rodada


main :: IO ()
main = do
  putStrLn "Boliche Arcade"
  putStrLn "Digite seu nome:"
  nome <- getLine
  putStrLn ("Olá " ++ nome ++ ", vamos começar!")
  putStrLn "Iniciando o jogo..."
  comecarJogo 0 iniciarPontuacao iniciarPontosPendentes
  -- inicio da parte grafica do gloss
  -- play
  --   janela
  --   black
  --   10
  --   informaRodada
  --   where
  --     janela = InWindow "Boliche Arcade" (820, 580) (50, 50)

