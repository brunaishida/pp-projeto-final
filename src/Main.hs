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
  putStrLn "Iniciando o jogo..."
  let pontuacao = iniciarPontuacaoMultiplayer nJogadoresInt
  let pontosCadaRodada = iniciarPontosCadaRodadaMultiplayer nJogadoresInt
  let pontosPendentes = iniciarPontosPendentesMultiplayer nJogadoresInt
  comecarJogoMultiplayer 0 nJogadoresInt nomesJogadores pontuacao pontosCadaRodada pontosPendentes

-- teste grafico
    -- display 
    --   janela
    --   white
    --   mundoUm
    --   where
    --     janela = (InWindow "Boliche Arcade" (1200, 680) (50, 50)) 
