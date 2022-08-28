module NovoJogo where
import PontosCadaRodada
import Pontuacao
import PontosPendentes
import Jogadores
import MultiplayerGame
import Input
import Bot

novoJogo :: IO ()
novoJogo = do
  putStrLn "Boliche Arcade"
  putStrLn "Digite seu nome:"
  nome <- getLine
  putStrLn ("Ola " ++ nome ++ ", vamos comecar!")
  tipoDeJogo <- validaBotOuMultiplayer 0
  if (tipoDeJogo == 0) then do
    let nomesJogadores = [nome, botName]
    let pontuacao = iniciarPontuacaoMultiplayer 2
    let pontosCadaRodada = iniciarPontosCadaRodadaMultiplayer 2
    let pontosPendentes = iniciarPontosPendentesMultiplayer 2
    comecarJogoMultiplayer 0 2 nomesJogadores pontuacao pontosCadaRodada pontosPendentes
  else do
    nJogadoresInt <- validaNJogadores 0
    nomesJogadores <- receberNomeJogadores nJogadoresInt [nome] nJogadoresInt
    putStrLn "Iniciando o jogo..."
    let pontuacao = iniciarPontuacaoMultiplayer nJogadoresInt
    let pontosCadaRodada = iniciarPontosCadaRodadaMultiplayer nJogadoresInt
    let pontosPendentes = iniciarPontosPendentesMultiplayer nJogadoresInt
    comecarJogoMultiplayer 0 nJogadoresInt nomesJogadores pontuacao pontosCadaRodada pontosPendentes
