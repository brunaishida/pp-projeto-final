module Ranking where
import Data.List
import Utils
import Pontuacao

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

