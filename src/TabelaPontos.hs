module TabelaPontos where
import Utils

trocaCaracteres1 :: (Int,Int) -> String
trocaCaracteres1 pontosCadaRodada =
  if ((fst pontosCadaRodada) == -1 || (fst pontosCadaRodada) == 10)
    then " "
    else show (fst pontosCadaRodada)

trocaCaracteres2 :: (Int,Int) -> String
trocaCaracteres2 pontosCadaRodada =
  if ((snd pontosCadaRodada) == -1)
    then " "
    else if ((fst pontosCadaRodada) == 10)
      then "X"
      else if ((somaPontosLance pontosCadaRodada) == 10)
        then "/"
        else show (snd pontosCadaRodada)

trocaCaracteresTotal :: [Int] -> Int -> Int -> (Int, Int) -> String
trocaCaracteresTotal pontuacaoArray (-1) pontTotal pontosCadaRodada = 
  if (pontTotal >= 10 && pontTotal < 100)
      then (show pontTotal ++ "  ")
      else if (pontTotal >= 100)
        then (show pontTotal ++ " ")
        else (show pontTotal ++ "   ")
trocaCaracteresTotal pontuacaoArray pos pontTotal pontosCadaRodada = 
  if ((pontuacaoArray!!pos == 0) && ((somaPontosLance pontosCadaRodada) /= 0))
    then "    "
    else (trocaCaracteresTotal pontuacaoArray (pos-1) (pontTotal + (pontuacaoArray!!pos)) (0,0))

trocaCaracteres10 :: (Int,Int) -> String
trocaCaracteres10 pontosCadaRodada = 
   if ((fst pontosCadaRodada) == -1)
    then " "
    else if ((fst pontosCadaRodada) == 10)
      then "X"
      else show (fst pontosCadaRodada)

imprimeTabela :: String -> [(Int,Int)] -> [Int] -> IO()
imprimeTabela nomeJogador pontosCadaRodada pontuacao = do 
  putStrLn(" ")
  putStrLn("-------------------------------" ++ nomeJogador ++ "-------------------------------")
  putStrLn("--------------------------------------------------------------------------------------")
  putStrLn("|   1   |   2   |   3   |   4   |   5   |   6   |   7   |   8   |   9   |     10    |")
  putStrLn(pos1 ++ pos2 ++ pos3 ++ pos4 ++ pos5 ++ pos6 ++ pos7 ++ pos8 ++ pos9 ++ verificaPos10)
  putStrLn(pont1 ++ pont2 ++ pont3 ++ pont4 ++ pont5 ++ pont6 ++ pont7 ++ pont8 ++ pont9 ++ pont10)
  putStrLn("--------------------------------------------------------------------------------------")
  putStrLn(" ")
  where
    pos1 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!0)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!0)) ++ " "
    pos2 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!1)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!1)) ++ " "
    pos3 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!2)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!2)) ++ " "
    pos4 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!3)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!3)) ++ " "
    pos5 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!4)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!4)) ++ " "
    pos6 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!5)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!5)) ++ " "
    pos7 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!6)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!6)) ++ " "
    pos8 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!7)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!7)) ++ " "
    pos9 = "| " ++ (trocaCaracteres1(pontosCadaRodada!!8)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!8)) ++ " "
    verificaPos10 = 
      if ((fst (pontosCadaRodada!!9) == 10) && (fst (pontosCadaRodada!!10) /= 10)) -- primeiro lance com strike e outros spare ou nao
        then "| " ++ (trocaCaracteres10(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres1(pontosCadaRodada!!10)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!10)) ++ " |"
        else if ((fst (pontosCadaRodada!!9) == 10) && (fst (pontosCadaRodada!!10) == 10)) -- dois strikes e o ultimo sem strike ou tres strikes
          then "| " ++ (trocaCaracteres10(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres10(pontosCadaRodada!!10)) ++ " " ++ "| " ++ (trocaCaracteres10(pontosCadaRodada!!11)) ++ " |"
          else if ((fst (pontosCadaRodada!!9) /= 10) && ((somaPontosLance (pontosCadaRodada!!9)) == 10) ) -- primeiro lance spare e outro nao strike ou strike
            then "| " ++ (trocaCaracteres1(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres10(pontosCadaRodada!!10)) ++ " |"
            else "| " ++ (trocaCaracteres1(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres2(pontosCadaRodada!!9)) ++ " " ++ "| " ++ (trocaCaracteres1(pontosCadaRodada!!10)) ++ " |"
    pont1 = "|   " ++ trocaCaracteresTotal pontuacao 0 0 (pontosCadaRodada!!0)
    pont2 = "|   " ++ trocaCaracteresTotal pontuacao 1 0 (pontosCadaRodada!!1)
    pont3 = "|   " ++ trocaCaracteresTotal pontuacao 2 0 (pontosCadaRodada!!2)
    pont4 = "|   " ++ trocaCaracteresTotal pontuacao 3 0 (pontosCadaRodada!!3)
    pont5 = "|   " ++ trocaCaracteresTotal pontuacao 4 0 (pontosCadaRodada!!4)
    pont6 = "|   " ++ trocaCaracteresTotal pontuacao 5 0 (pontosCadaRodada!!5)
    pont7 = "|   " ++ trocaCaracteresTotal pontuacao 6 0 (pontosCadaRodada!!6)
    pont8 = "|   " ++ trocaCaracteresTotal pontuacao 7 0 (pontosCadaRodada!!7)
    pont9 = "|   " ++ trocaCaracteresTotal pontuacao 8 0 (pontosCadaRodada!!8)
    pont10 = "|     " ++ trocaCaracteresTotal pontuacao 9 0  (pontosCadaRodada!!9) ++ "  |"
 