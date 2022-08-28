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
    -- print pinos
    return pinos
  else do
    -- print pinos
    pinoParaDerrubar <- getStdRandom $ randomR (0, 9 :: Integer)
    -- putStrLn("Pino Derrubado!")
    -- print pinoParaDerrubar
    resultadoJogada (pinosParaDerrubar-1) (derrubarPino pinoParaDerrubar pinos)

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
  pontuacaoLance <- getStdRandom $ randomR (0, 9 :: Int)
  putStrLn("Você acertou "++ (intToString pontuacaoLance) ++ " pinos!!")
  putStrLn("---------------Total Lance = " ++ (intToString pontuacaoLance) ++ " pontos ---------------")
  let valorRodada = (pontuacaoLance, 0)
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
