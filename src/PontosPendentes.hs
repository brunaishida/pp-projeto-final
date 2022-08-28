module PontosPendentes where
import Data.List
import Utils

iniciarPontosPendentes :: [(Int, Int)]
iniciarPontosPendentes = [(-1,-1) | x <- [0..11], True]

iniciarPontosPendentesMultiplayer :: Int -> [[(Int, Int)]]
iniciarPontosPendentesMultiplayer n = [iniciarPontosPendentes | x <- [0..(n-1)], True]

adicionarPontoPendente :: [(Int, Int)] -> (Int, Int) -> Integer -> [(Int, Int)]
adicionarPontoPendente pontosPendentes novoPonto posicao = take (integerToInt posicao) pontosPendentes ++ novoPonto : drop ((integerToInt posicao)+1) pontosPendentes

adicionarPontosPendentesJogadores :: [[(Int, Int)]] -> [(Int, Int)] -> Int -> [[(Int, Int)]]
adicionarPontosPendentesJogadores pontosPendentesJogadores novoPontosPendentes posicao = 
  take posicao pontosPendentesJogadores ++ novoPontosPendentes : drop (posicao+1) pontosPendentesJogadores

zerarPontoPendente :: [(Int, Int)] -> Integer -> [(Int, Int)]
zerarPontoPendente pontosPendentes posicao = adicionarPontoPendente pontosPendentes (-1,-1) posicao

pontuacaoPendentePreenchida :: (Int, Int) -> Bool
pontuacaoPendentePreenchida pontuacaoPendente = (fst pontuacaoPendente) > -1
