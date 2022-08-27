module Desenho where
    
import Graphics.Gloss

-- informaRodada :: Integer -> Picture
-- informaRodada rodada = 
    -- pictures translate (0) -200 ( scale 0.3 0.3 $
--     color (greyN 0.5) $
--     (Text "Rodada: " ++ show (rodada)))
-- verificar coords ideais pra fazer cada retangulo do score + decidir se havera borda e localizacao dos textos

caixaScore :: Picture
caixaScore = pictures $ [translate (0) (50) $ color (greyN 0.8) $ rectangleSolid 100 20]
                     ++ [translate (0) (50) $ rectangleWire 100 20]
                     ++ [translate (0) (0) $ rectangleWire 100 80]
                     ++ [translate (25) (20) $ rectangleWire 50 40]

caixaUltimoScore :: Picture
caixaUltimoScore = pictures $ [translate (0) (50) $ color (greyN 0.8) $ rectangleSolid 100 20]
                     ++ [translate (0) (50) $ rectangleWire 100 20]
                     ++ [translate (0) (0) $ rectangleWire 100 80]
                     ++ [translate (33.2) (20) $ rectangleWire 33 40] -- caixa direita
                     ++ [translate (0) (20) $ rectangleWire 33 40] -- caixa esquerda

tabelaScore :: Picture
tabelaScore = pictures $ [translate (-450) (0) $ caixaScore] -- 1
                    ++ [translate (-350) (0) $ caixaScore] -- 2
                    ++ [translate (-250) (0) $ caixaScore] -- 3
                    ++ [translate (-150) (0) $ caixaScore] -- 4
                    ++ [translate (-50) (0) $ caixaScore] -- 5
                    ++ [translate (50) (0) $ caixaScore] -- 6
                    ++ [translate (150) (0) $ caixaScore] -- 7
                    ++ [translate (250) (0) $ caixaScore] -- 8
                    ++ [translate (350) (0) $ caixaScore] -- 9
                    ++ [translate (450) (0) $ caixaUltimoScore] -- 10

numeroRound :: Picture
numeroRound = pictures $ [translate (-455) (45) $ scale 0.1 0.1 $ text "1"] -- 1
                    ++ [translate (-355) (45) $ scale 0.1 0.1 $ text "2"] -- 2
                    ++ [translate (-255) (45) $ scale 0.1 0.1 $ text "3"] -- 3
                    ++ [translate (-155) (45) $ scale 0.1 0.1 $ text "4"] -- 4
                    ++ [translate (-55) (45) $ scale 0.1 0.1 $ text "5"] -- 5
                    ++ [translate (45) (45) $ scale 0.1 0.1 $ text "6"] -- 6
                    ++ [translate (145) (45) $ scale 0.1 0.1 $ text "7"] -- 7
                    ++ [translate (245) (45) $ scale 0.1 0.1 $ text "8"] -- 8
                    ++ [translate (345) (45) $ scale 0.1 0.1 $ text "9"] -- 9
                    ++ [translate (445) (45) $ scale 0.1 0.1 $ text "10"] -- 10

inicioJogo :: Picture
inicioJogo = pictures $ [translate (-130) (120) $ scale 0.3 0.3 $ text "Boliche Arcade"]
                ++ [translate (-170) (-100) $ scale 0.2 0.2 $ text "Para iniciar, digite seu nome: "]


fimJogo :: Picture
fimJogo = pictures $ [translate (-130) (120) $ scale 0.3 0.3 $ text "Fim de Jogo!"]
                ++ [translate (-100) (-100) $ scale 0.2 0.2 $ text "Pontuacao final: "]

mundo :: Picture
mundo = pictures $ [tabelaScore]
            ++ [numeroRound]
            ++ [inicioJogo]