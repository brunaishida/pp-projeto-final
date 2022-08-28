module Jogadores where
import Text.Read
import Utils
import Bot

adicionaNomeJogador :: [String] -> String -> [String] 
adicionaNomeJogador [] nome = [nome]
adicionaNomeJogador (nomeJogador:nomesJogadores) nome = nomeJogador : (adicionaNomeJogador nomesJogadores nome)

receberNomeJogadores :: Int -> [String] -> Int -> IO [String]
receberNomeJogadores nJogadores nomesJogadores 1 =do return nomesJogadores
receberNomeJogadores nJogadores nomesJogadores counter= do
  putStrLn("Adicione o nome do jogador " ++ (intToString (nJogadores-counter+2)))
  nomeJogador <- getLine
  if (ehBot nomeJogador) then do
    putStrLn("Nome invalido, digite novamente.")
    receberNomeJogadores nJogadores nomesJogadores counter
  else do
    let nomesJogadoresAtualizado = (adicionaNomeJogador nomesJogadores nomeJogador)
    receberNomeJogadores nJogadores nomesJogadoresAtualizado (counter-1)

validaNJogadores :: Int -> IO Int
validaNJogadores count = do
  if(count==0) then do
    putStrLn ("Quantas pessoas vao jogar? Contando com você. Digite um número de 2-4.")
    nJogadores <- getLine
    let nJogadoresMaybe = (readMaybe nJogadores) :: Maybe Int
    if (nJogadoresMaybe == Nothing) then do
        validaNJogadores (count+1)
    else do
      let valor = puroJust nJogadoresMaybe
      if (valor >= 2 && valor <= 4) then do
        return valor
      else do
        validaNJogadores (count+1)
  else do
    putStrLn("Numero invalido, digite um numero de 2-4")
    nJogadores <- getLine
    let nJogadoresMaybe = (readMaybe nJogadores) :: Maybe Int
    if ( nJogadoresMaybe == Nothing) then do
        validaNJogadores (count+1)
    else do
      let valor = puroJust nJogadoresMaybe
      if (valor >= 2 && valor <= 4) then do
        return valor
      else do
        validaNJogadores (count+1)
