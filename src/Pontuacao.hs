module Pontuacao where
import Data.List
import Utils
import Pinos
import PontosPendentes

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

calculaPontuacao :: [String] -> [String] -> Int
calculaPontuacao pinosPosicaoAnterior pinosNovaPosicao = (calculaPontos (filtraPinosNovosDerrubados pinosPosicaoAnterior pinosNovaPosicao))

pontuacaoTotal :: [[Int]] -> [Int]
pontuacaoTotal [] = []
pontuacaoTotal (x:xs) = somaTotalPotuacao x : pontuacaoTotal xs

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