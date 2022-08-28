module Main where
import PontosCadaRodada
import Pontuacao
import PontosPendentes
import Jogadores
import MultiplayerGame

main :: IO ()
main = do
  putStrLn "Boliche Arcade"
  putStrLn "Digite seu nome:"
  nome <- getLine
  putStrLn ("Olá " ++ nome ++ ", vamos começar!")
  nJogadoresInt <- validaNJogadores 0
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
