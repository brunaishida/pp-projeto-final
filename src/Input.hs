module Input where
import Text.Read
import Utils

validaBotOuMultiplayer :: Int -> IO Int
validaBotOuMultiplayer count = do
  if(count==0) then do
    putStrLn ("Voce ira jogar contra pessoas ou um bot? Digite 0 para bot e 1 para multiplayer.")
    tipoJogo <- getLine
    let tipoJogoMaybe = (readMaybe tipoJogo) :: Maybe Int
    if (tipoJogoMaybe == Nothing) then do
        validaBotOuMultiplayer (count+1)
    else do
      let valor = puroJust tipoJogoMaybe
      if (valor >= 0 && valor <= 1) then do
        return valor
      else do
        validaBotOuMultiplayer (count+1)
  else do
    putStrLn("Numero invalido. Digite 0 para bot e 1 para multiplayer")
    tipoJogo <- getLine
    let tipoJogoMaybe = (readMaybe tipoJogo) :: Maybe Int
    if ( tipoJogoMaybe == Nothing) then do
        validaBotOuMultiplayer (count+1)
    else do
      let valor = puroJust tipoJogoMaybe
      if (valor >= 2 && valor <= 4) then do
        return valor
      else do
        validaBotOuMultiplayer (count+1)
