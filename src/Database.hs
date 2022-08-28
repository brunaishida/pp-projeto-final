module Database where

import System.Random
import Data.List
import Graphics.Gloss

-- data Frames = 
--     Frames {
--         frameAtual :: Int , -- mesma coisa que a rodada atual
--         primeiroPinos :: Int , 
--         segundoPinos :: Int , 
--         gameOver :: Bool
--     }
--     deriving Eq

-- parametros de teste do display
pontuacaoTotalRodada :: Int
pontuacaoTotalRodada = 2

jogadaUmRodada1 ::  Int
jogadaUmRodada1 = 5

jogadaDoisRodada1 :: Int
jogadaDoisRodada1 = 7
