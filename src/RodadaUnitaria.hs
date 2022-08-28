module RodadaUnitaria where
import System.Random
import Utils
import PontosPendentes
import PontosCadaRodada
import Pinos
import Pontuacao

resultadoJogada :: Integer -> [String] -> IO [String]
resultadoJogada pinosParaDerrubar pinos = do
  if (pinosParaDerrubar <= 0) then do
    return pinos
  else do
    pinoParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
    resultadoJogada (pinosParaDerrubar-1) (derrubarPino pinoParaDerrubar pinos)

iniciarLance :: [String] -> Int -> Integer -> IO (Int, Int)
iniciarLance pinos pontuacao nLance = do
  if (nLance == 1) then do
    putStrLn(" ")
    putStrLn("Lance numero 2")
    putStrLn("Aperte enter para jogar a bola!")
    jogada <- getLine
    pinosParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
    resultadoPinos <- (resultadoJogada pinosParaDerrubar pinos)
    let totalPinosDerrubados = (calculaPontuacao pinos resultadoPinos)
    let totalPinosDerrubadosRodada = (pontuacao + totalPinosDerrubados)
    if ( totalPinosDerrubadosRodada == 10) then do
      putStrLn("Spare!!!!!!")
      putStrLn("Voce acertou "++ (intToString totalPinosDerrubados) ++ " pinos!!")
      putStrLn("---------------Total Lance = " ++ (intToString totalPinosDerrubadosRodada) ++ " pontos ---------------")
      return (pontuacao, totalPinosDerrubados)
    else do
      putStrLn("Voce acertou "++ (intToString totalPinosDerrubados) ++ " pinos!!")
      putStrLn("---------------Total Lance = " ++ (intToString totalPinosDerrubadosRodada) ++ " pontos ---------------")
      return (pontuacao, totalPinosDerrubados)
  else do
      putStrLn(" ")
      putStrLn("Lance numero 1")
      putStrLn("Aperte enter para jogar a bola!")
      jogada <- getLine
      pinosParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
      if (pinosParaDerrubar == 9) then do 
        putStrLn("Strike!!!!!!!")
        putStrLn("Voce acertou 10 pinos!!")
        return (10,0) -- saberemos que é um Strike
      else do
        resultadoPinos <- (resultadoJogada pinosParaDerrubar pinos)
        let totalPinosDerrubados = (calculaPontuacao pinos resultadoPinos)
        putStrLn("Voce acertou "++ (intToString totalPinosDerrubados) ++ " pinos!!")
        iniciarLance resultadoPinos totalPinosDerrubados (nLance+1)

iniciarLanceBot :: [String] -> Int -> Integer -> IO (Int, Int)
iniciarLanceBot pinos pontuacao nLance = do
  if (nLance == 1) then do
    putStrLn(" ")
    putStrLn("Lance numero 2")
    putStrLn("O bot jogou a bola!")
    pinosParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
    resultadoPinos <- (resultadoJogada pinosParaDerrubar pinos)
    let totalPinosDerrubados = (calculaPontuacao pinos resultadoPinos)
    putStrLn("O bot acertou "++ (intToString totalPinosDerrubados) ++ " pinos!!")
    putStrLn("---------------Total Lance = " ++ (intToString (pontuacao + totalPinosDerrubados)) ++ " pontos ---------------")
    return (pontuacao, totalPinosDerrubados)
  else do
      putStrLn(" ")
      putStrLn("Lance numero 1")
      putStrLn("O bot jogou a bola!")
      pinosParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
      if (pinosParaDerrubar == 9) then do 
        putStrLn("Strike!!!!!!!")
        putStrLn("O bot acertou 10 pinos!!")
        return (10,0) -- saberemos que é um Strike
      else do
        resultadoPinos <- (resultadoJogada pinosParaDerrubar pinos)
        let totalPinosDerrubados = (calculaPontuacao pinos resultadoPinos)
        putStrLn("O bot acertou "++ (intToString totalPinosDerrubados) ++ " pinos!!")
        iniciarLanceBot resultadoPinos totalPinosDerrubados (nLance+1)

casoRodada10 :: [(Int, Int)] -> [(Int, Int)] -> Integer -> IO ([(Int, Int)], [(Int, Int)]) 
casoRodada10 pontosPendentes pontosCadaRodada 0 = do
  putStrLn("Aperte enter para jogar a bola!")
  jogada <- getLine
  pontuacaoLance <- getStdRandom $ randomR (0, 9 :: Int)
  putStrLn("Voce acertou "++ (intToString pontuacaoLance) ++ " pinos!!")
  putStrLn("---------------Total Lance = " ++ (intToString pontuacaoLance) ++ " pontos ---------------")
  let valorRodada = (pontuacaoLance, 0)
  let novosPontosPendentes = (adicionarPontoPendente pontosPendentes valorRodada 11)
  let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada valorRodada 11)
  return (novosPontosPendentes, novosPontosCadaRodada)
casoRodada10 pontosPendentes pontosCadaRodada 1 = do 
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
  putStrLn("Aperte enter para jogar a bola!")
  jogada <- getLine
  pontuacaoLance <- (iniciarLance iniciarPinos 0 0)
  if (fst pontuacaoLance == 10) then do
    let valorRodada = (10, 0)
    let novosPontosPendentes = (adicionarPontoPendente pontosPendentes valorRodada 9)
    let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada valorRodada 9)
    casoRodada10 novosPontosPendentes novosPontosCadaRodada 1
  else do
    let novosPontosPendentes = (adicionarPontoPendente pontosPendentes pontuacaoLance 9)
    let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada pontuacaoLance 9)
    if (somaPontosLance pontuacaoLance == 10) then do
      casoRodada10 novosPontosPendentes novosPontosCadaRodada 0
    else do
      return (novosPontosPendentes, novosPontosCadaRodada)


casoRodada10Bot :: [(Int, Int)] -> [(Int, Int)] -> Integer -> IO ([(Int, Int)], [(Int, Int)]) 
casoRodada10Bot pontosPendentes pontosCadaRodada 0 = do
  pontuacaoLance <- getStdRandom $ randomR (0, 9 :: Int)
  putStrLn("O bot acertou "++ (intToString pontuacaoLance) ++ " pinos!!")
  putStrLn("---------------Total Lance = " ++ (intToString pontuacaoLance) ++ " pontos ---------------")
  let valorRodada = (pontuacaoLance, 0)
  let novosPontosPendentes = (adicionarPontoPendente pontosPendentes valorRodada 11)
  let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada valorRodada 11)
  return (novosPontosPendentes, novosPontosCadaRodada)
casoRodada10Bot pontosPendentes pontosCadaRodada 1 = do 
  pontuacaoLance <- (iniciarLanceBot iniciarPinos 0 0)
  if (somaPontosLance pontuacaoLance == 10) then do
      let valorRodada = (10, 0)
      let novosPontosPendentes = (adicionarPontoPendente pontosPendentes valorRodada 10)
      let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada valorRodada 10)
      casoRodada10Bot novosPontosPendentes novosPontosCadaRodada 0
  else do
      let novosPontosPendentes = (adicionarPontoPendente pontosPendentes pontuacaoLance 10)
      let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada pontuacaoLance 10)
      return (novosPontosPendentes, novosPontosCadaRodada)
casoRodada10Bot pontosPendentes pontosCadaRodada 2 = do
  pontuacaoLance <- (iniciarLanceBot iniciarPinos 0 0)
  if (fst pontuacaoLance == 10) then do
    let valorRodada = (10, 0)
    let novosPontosPendentes = (adicionarPontoPendente pontosPendentes valorRodada 9)
    let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada valorRodada 9)
    casoRodada10Bot novosPontosPendentes novosPontosCadaRodada 1
  else do
    let novosPontosPendentes = (adicionarPontoPendente pontosPendentes pontuacaoLance 9)
    let novosPontosCadaRodada = (adicionarPontoDaRodada pontosCadaRodada pontuacaoLance 9)
    if (somaPontosLance pontuacaoLance == 10) then do
      casoRodada10Bot novosPontosPendentes novosPontosCadaRodada 0
    else do
      return (novosPontosPendentes, novosPontosCadaRodada)

