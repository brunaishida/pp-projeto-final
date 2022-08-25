module Database where

import Graphics.Gloss

-- Coordenada (x, y) - para situar os retangulos do score
type Coord = (Float, Float)

data Frames = 
    Frames {
        frameAtual :: Int , -- mesma coisa que a rodada atual
        primeiroPinos :: Int , 
        segundoPinos :: Int , 
        gameOver :: Bool
    }
    deriving Eq