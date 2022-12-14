module MultiplayerGame where
import Utils
import TabelaPontos
import Pinos
import Ranking
import Jogadores
import Pontuacao
import PontosPendentes
import PontosCadaRodada
import RodadaUnitaria
import Bot
import NovoJogo

comecarJogoMultiplayer :: Integer -> Int -> [String] -> [[Int]] -> [[(Int, Int)]] -> [[(Int, Int)]] -> IO()
comecarJogoMultiplayer rodada numJogadores nomesJogadores pontuacao pontosCadaRodada pontosPendentes = do
  if (rodada > 9) then do
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

iniciarRodadaParaCadaJogador :: Integer -> Int -> [String] -> [[Int]] -> [[(Int, Int)]] -> [[(Int, Int)]] -> Int -> IO([[Int]], [[(Int, Int)]], [[(Int, Int)]])
iniciarRodadaParaCadaJogador rodada numJogadores nomesJogadores pontuacao pontosCadaRodada pontosPendentes counter = do
  if (counter == numJogadores) then do
    return (pontuacao, pontosCadaRodada, pontosPendentes)
  else do
    putStrLn(" ")
    putStrLn("*** Agora eh a vez de "++ (nomesJogadores!!counter) ++ " ***")
    let pontuacaoDoJogador = pontuacao!!counter
    let pontosPendentesDoJogador = pontosPendentes!!counter
    let pontosCadaRodadaDoJogador = pontosCadaRodada!!counter
    let nomeDoJogador = nomesJogadores!!counter
    if (ehBot nomeDoJogador) then do
      pontuacaoLance <- (iniciarLanceBot iniciarPinos 0 0)
      atualizaFunc pontuacaoDoJogador pontosPendentesDoJogador pontuacaoLance pontosCadaRodadaDoJogador nomeDoJogador
    else do
      pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
      atualizaFunc pontuacaoDoJogador pontosPendentesDoJogador pontuacaoLance pontosCadaRodadaDoJogador nomeDoJogador
    where 
      atualizaFunc pontuacaoDoJogador pontosPendentesDoJogador pontuacaoLance pontosCadaRodadaDoJogador nomeDoJogador = do    
        let (novaPontuacaoDoJogador, novosPontosPendentesDoJogador) = (calcularNovaPontuacao pontuacaoDoJogador (adicionarPontoPendente pontosPendentesDoJogador pontuacaoLance rodada) 0)
        let novosPontosCadaRodadaDoJogador = (adicionarPontoDaRodada pontosCadaRodadaDoJogador pontuacaoLance rodada)
        imprimeTabela nomeDoJogador novosPontosCadaRodadaDoJogador novaPontuacaoDoJogador
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
    putStrLn("*** Agora eh a vez de "++ (nomesJogadores!!counter) ++ " ***")
    let nomeDoJogador = nomesJogadores!!counter
    let pontuacaoDoJogador = pontuacao!!counter
    let pontosPendentesDoJogador = pontosPendentes!!counter
    let pontosCadaRodadaDoJogador = pontosCadaRodada!!counter
    if (ehBot nomeDoJogador) then do
      resultadoPontosPendentes <- (casoRodada10Bot pontosPendentesDoJogador pontosCadaRodadaDoJogador 2)
      atualizaFunc pontuacaoDoJogador pontosPendentesDoJogador resultadoPontosPendentes pontosCadaRodadaDoJogador nomeDoJogador
    else do
      resultadoPontosPendentes <- casoRodada10 pontosPendentesDoJogador pontosCadaRodadaDoJogador 2
      atualizaFunc pontuacaoDoJogador pontosPendentesDoJogador resultadoPontosPendentes  pontosCadaRodadaDoJogador nomeDoJogador
    where 
      atualizaFunc pontuacaoDoJogador pontosPendentesDoJogador resultadoPontosPendentes pontosCadaRodadaDoJogador nomeDoJogador = do    
        let (novosPontosPendentesDoJogador1, novosPontosCadaRodadaDoJogador) = resultadoPontosPendentes
        let (novaPontuacaoDoJogador, novosPontosPendentesDoJogador) = (calcularNovaPontuacao pontuacaoDoJogador novosPontosPendentesDoJogador1 0)
        let novaPontuacaoJogadores = adicionarPontuacaoJogadores pontuacao novaPontuacaoDoJogador counter
        let novaPontuacaoPendenteJogadores = adicionarPontosPendentesJogadores pontosPendentes novosPontosPendentesDoJogador counter
        let novaPontuacaoCadaRodadaJogadores = adicionarPontoCadaRodadaJogadores pontosCadaRodada novosPontosCadaRodadaDoJogador counter
        imprimeTabela nomeDoJogador novosPontosCadaRodadaDoJogador novaPontuacaoDoJogador
        iniciarRodada10ParaCadaJogador rodada numJogadores nomesJogadores novaPontuacaoJogadores novaPontuacaoCadaRodadaJogadores novaPontuacaoPendenteJogadores (counter+1)
