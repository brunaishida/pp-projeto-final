module Jogadores where
import Text.Read
import Utils

adicionaNomeJogador :: [String] -> String -> [String] 
adicionaNomeJogador [] nome = [nome]
adicionaNomeJogador (nomeJogador:nomesJogadores) nome = nomeJogador : (adicionaNomeJogador nomesJogadores nome)

receberNomeJogadores :: Int -> [String] -> Int -> IO [String]
receberNomeJogadores nJogadores nomesJogadores 1 =do return nomesJogadores
receberNomeJogadores nJogadores nomesJogadores counter= do
  putStrLn("Adicione o nome do jogador " ++ (intToString (nJogadores-counter+2)))
  nomeJogador <- getLine
  receberNomeJogadores nJogadores (adicionaNomeJogador nomesJogadores nomeJogador) (counter-1)

validaNJogadores :: Int -> IO Int
validaNJogadores count = do
  if(count==0) then do
    putStrLn ("Quantas pessoas vão jogar? Contando com você. Digite um número de 2-4.")
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
    putStrLn("Número inválido, digite um número de 2-4")
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
