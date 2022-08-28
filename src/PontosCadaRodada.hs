module PontosCadaRodada where
import Data.List
import Utils

iniciarPontosCadaRodada :: [(Int, Int)]
iniciarPontosCadaRodada = [(-1,-1) | x <- [0..11], True]

iniciarPontosCadaRodadaMultiplayer :: Int -> [[(Int, Int)]]
iniciarPontosCadaRodadaMultiplayer n = [iniciarPontosCadaRodada | x <- [0..(n-1)], True]

adicionarPontoDaRodada :: [(Int, Int)] -> (Int, Int) -> Integer -> [(Int, Int)]
adicionarPontoDaRodada pontosDaRodada novoPonto posicao = take (integerToInt posicao) pontosDaRodada ++ novoPonto : drop ((integerToInt posicao)+1) pontosDaRodada

adicionarPontoCadaRodadaJogadores :: [[(Int, Int)]] -> [(Int, Int)] -> Int -> [[(Int, Int)]]
adicionarPontoCadaRodadaJogadores pontosCadaRodadaJogadores novoPonto posicao = take posicao pontosCadaRodadaJogadores ++ novoPonto : drop (posicao+1) pontosCadaRodadaJogadores
