module Desenho where
    
import Graphics.Gloss
import Database

desenhaBola :: Picture
desenhaBola = pictures $ [translate (0) (20) $ circleSolid 50]
                    ++ [translate (20) (30) $ color white $ circleSolid 5]
                    ++ [translate (2) (30) $ color white $ circleSolid 5]
                    ++ [translate (11) (5) $ color white $ circleSolid 5]

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
inicioJogo = pictures $ [translate (-130) (100) $ scale 0.3 0.3 $ text "Boliche Arcade"]
                ++ [translate (-125) (-70) $ scale 0.2 0.2 $ text "Para iniciar o jogo,"]
                ++ [translate (-265) (-120) $ scale 0.2 0.2 $ text "digite a quantidade de jogadores (1 ou 2)"]
                ++ [translate (-105) (150) $ scale 0.2 0.2 $ text "Bem vindo(a) ao"]
                ++ [desenhaBola]

instrucoes :: Picture
instrucoes = pictures $ [translate (-300) (-100) $ scale 0.2 0.2 $ text "O Boliche Arcade possui jogadas aleatorias."]
                    ++ [translate (-350) (-150) $ scale 0.2 0.2 $ text "Player 1: Para jogar, pressione a seta para cima."]

fimJogoUm :: Picture
fimJogoUm = pictures $ [translate (-130) (-220) $ scale 0.3 0.3 $ text "Fim de Jogo!"]
                ++ [translate (-130) (-260) $ scale 0.2 0.2 $ text pont1]
                where
                    pont1 = "Pontuacao final: " ++ show pontuacaoTotalRodada

fimJogoDois :: Picture
fimJogoDois = pictures $ [translate (-120) (15) $ scale 0.3 0.3 $ text "Fim de Jogo!"]
                ++ [translate (-100) (-15) $ scale 0.2 0.2 $ text pont1]
                ++ [translate (30) (-15) $ scale 0.2 0.2 $ text pont2]
                where
                    pont1 = "P1: " ++ show pontuacaoTotalRodada
                    pont2 = "P2: " ++ show pontuacaoTotalRodada

scoreRound1 :: Picture
scoreRound1 = pictures $ [translate (-480) (10) $ scale 0.2 0.2 $ text (show jogadaUmRodada1)] -- show pontosDeCadaRodada!!0 
                    ++ [translate (-430) (10) $ scale 0.2 0.2 $ text (show jogadaDoisRodada1)]
                    ++ [translate (-455) (-30) $ scale 0.2 0.2 $ text (show 2)]

scoreRound2 :: Picture
scoreRound2 = pictures $ [translate (-380) (10) $ scale 0.2 0.2 $ text "3"] -- show pontosDeCadaRodada 
                    ++ [translate (-330) (10) $ scale 0.2 0.2 $ text "4"]
                    ++ [translate (-355) (-30) $ scale 0.2 0.2 $ text (show 1)]

scoreRound3 :: Picture
scoreRound3 = pictures $ [translate (-280) (10) $ scale 0.2 0.2 $ text "5"] -- show pontosDeCadaRodada 
                    ++ [translate (-230) (10) $ scale 0.2 0.2 $ text "6"]
                    ++ [translate (-255) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound4 :: Picture
scoreRound4 = pictures $ [translate (-180) (10) $ scale 0.2 0.2 $ text "7"] -- show pontosDeCadaRodada 
                    ++ [translate (-130) (10) $ scale 0.2 0.2 $ text "8"]
                    ++ [translate (-155) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound5 :: Picture
scoreRound5 = pictures $ [translate (-80) (10) $ scale 0.2 0.2 $ text "9"] -- show pontosDeCadaRodada 
                    ++ [translate (-30) (10) $ scale 0.2 0.2 $ text "/"]
                    ++ [translate (-55) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound6 :: Picture
scoreRound6 = pictures $ [translate (20) (10) $ scale 0.2 0.2 $ text ""] -- show pontosDeCadaRodada 
                    ++ [translate (70) (10) $ scale 0.2 0.2 $ text "X"]
                    ++ [translate (45) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound7 :: Picture
scoreRound7 = pictures $ [translate (120) (10) $ scale 0.2 0.2 $ text "2"] -- show pontosDeCadaRodada 
                    ++ [translate (170) (10) $ scale 0.2 0.2 $ text "6"]
                    ++ [translate (145) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound8 :: Picture
scoreRound8 = pictures $ [translate (220) (10) $ scale 0.2 0.2 $ text "9"] -- show pontosDeCadaRodada 
                    ++ [translate (270) (10) $ scale 0.2 0.2 $ text "0"]
                    ++ [translate (245) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound9 :: Picture
scoreRound9 = pictures $ [translate (320) (10) $ scale 0.2 0.2 $ text "6"] -- show pontosDeCadaRodada 
                    ++ [translate (370) (10) $ scale 0.2 0.2 $ text "5"]
                    ++ [translate (345) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound10 :: Picture
scoreRound10 = pictures $ [translate (410) (10) $ scale 0.2 0.2 $ text "3"] -- show pontosDeCadaRodada 
                    ++ [translate (445) (10) $ scale 0.2 0.2 $ text "1"]
                    ++ [translate (475) (10) $ scale 0.2 0.2 $ text "9"]
                    ++ [translate (445) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]            

jogadorUm :: Picture
jogadorUm = pictures $ [translate (0) (100) $ tabelaScore] -- P1
                    ++ [translate (0) (100) $ numeroRound] -- P1
                    ++ [translate (-570) (100) $ scale 0.3 0.3 $ color red $ text "P1"]
                    ++ [translate (0) (100) $ scoreRound1]
                    ++ [translate (0) (100) $ scoreRound2]
                    ++ [translate (0) (100) $ scoreRound3]
                    ++ [translate (0) (100) $ scoreRound4]
                    ++ [translate (0) (100) $ scoreRound5]
                    ++ [translate (0) (100) $ scoreRound6]
                    ++ [translate (0) (100) $ scoreRound7]
                    ++ [translate (0) (100) $ scoreRound8]
                    ++ [translate (0) (100) $ scoreRound9]
                    ++ [translate (0) (100) $ scoreRound10]

scoreRound1P2 :: Picture
scoreRound1P2 = pictures $ [translate (-480) (10) $ scale 0.2 0.2 $ text "1"] -- show pontosDeCadaRodada 
                    ++ [translate (-430) (10) $ scale 0.2 0.2 $ text "2"]
                    ++ [translate (-455) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound2P2 :: Picture
scoreRound2P2 = pictures $ [translate (-380) (10) $ scale 0.2 0.2 $ text "3"] -- show pontosDeCadaRodada 
                    ++ [translate (-330) (10) $ scale 0.2 0.2 $ text "4"]
                    ++ [translate (-355) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound3P2 :: Picture
scoreRound3P2 = pictures $ [translate (-280) (10) $ scale 0.2 0.2 $ text "5"] -- show pontosDeCadaRodada 
                    ++ [translate (-230) (10) $ scale 0.2 0.2 $ text "6"]
                    ++ [translate (-255) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound4P2 :: Picture
scoreRound4P2 = pictures $ [translate (-180) (10) $ scale 0.2 0.2 $ text "7"] -- show pontosDeCadaRodada 
                    ++ [translate (-130) (10) $ scale 0.2 0.2 $ text "8"]
                    ++ [translate (-155) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound5P2 :: Picture
scoreRound5P2 = pictures $ [translate (-80) (10) $ scale 0.2 0.2 $ text "9"] -- show pontosDeCadaRodada 
                    ++ [translate (-30) (10) $ scale 0.2 0.2 $ text "/"]
                    ++ [translate (-55) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound6P2 :: Picture
scoreRound6P2 = pictures $ [translate (20) (10) $ scale 0.2 0.2 $ text ""] -- show pontosDeCadaRodada 
                    ++ [translate (70) (10) $ scale 0.2 0.2 $ text "X"]
                    ++ [translate (45) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound7P2 :: Picture
scoreRound7P2 = pictures $ [translate (120) (10) $ scale 0.2 0.2 $ text "2"] -- show pontosDeCadaRodada 
                    ++ [translate (170) (10) $ scale 0.2 0.2 $ text "6"]
                    ++ [translate (145) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound8P2 :: Picture
scoreRound8P2 = pictures $ [translate (220) (10) $ scale 0.2 0.2 $ text "9"] -- show pontosDeCadaRodada 
                    ++ [translate (270) (10) $ scale 0.2 0.2 $ text "0"]
                    ++ [translate (245) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound9P2 :: Picture
scoreRound9P2 = pictures $ [translate (320) (10) $ scale 0.2 0.2 $ text "6"] -- show pontosDeCadaRodada 
                    ++ [translate (370) (10) $ scale 0.2 0.2 $ text "5"]
                    ++ [translate (345) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]

scoreRound10P2 :: Picture
scoreRound10P2 = pictures $ [translate (410) (10) $ scale 0.2 0.2 $ text "3"] -- show pontosDeCadaRodada 
                    ++ [translate (445) (10) $ scale 0.2 0.2 $ text "1"]
                    ++ [translate (475) (10) $ scale 0.2 0.2 $ text "9"]
                    ++ [translate (445) (-30) $ scale 0.2 0.2 $ text (show pontuacaoTotalRodada)]                            

jogadorDois :: Picture
jogadorDois = pictures $ [translate (0) (-100) $ tabelaScore] -- P2
                    ++ [translate (0) (-100) $ numeroRound] -- P2
                    ++ [translate (-570) (-100) $ scale 0.3 0.3 $ color blue $ text "P2"]
                    ++ [translate (0) (-100) $ scoreRound1P2]
                    ++ [translate (0) (-100) $ scoreRound2P2]
                    ++ [translate (0) (-100) $ scoreRound3P2]
                    ++ [translate (0) (-100) $ scoreRound4P2]
                    ++ [translate (0) (-100) $ scoreRound5P2]
                    ++ [translate (0) (-100) $ scoreRound6P2]
                    ++ [translate (0) (-100) $ scoreRound7P2]
                    ++ [translate (0) (-100) $ scoreRound8P2]
                    ++ [translate (0) (-100) $ scoreRound9P2]
                    ++ [translate (0) (-100) $ scoreRound10P2]

mundoVazio :: Picture
mundoVazio = pictures $ [inicioJogo]

mundoUm :: Picture
mundoUm = pictures $ [translate (-130) (100) $ scale 0.3 0.3 $ text "Boliche Arcade"]
            ++ [translate (0) (-100) $ jogadorUm]
            ++ [instrucoes]
            ++ [fimJogoUm]

mundoDois :: Picture
mundoDois = pictures $ [translate (-130) (200) $ scale 0.3 0.3 $ text "Boliche Arcade"]
                    ++ [jogadorUm]
                    ++ [jogadorDois]
                    ++ [translate (0) (-100) $ instrucoes]
                    ++ [translate (-350) (-300) $ scale 0.2 0.2 $ text "Player 2: Para jogar, pressione a seta para baixo."]
                    ++ [fimJogoDois]