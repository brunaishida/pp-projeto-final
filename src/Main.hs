module Main where
import System.Random

iniciarPinos :: [String]
iniciarPinos = ["0" | x <- [0..9], True]

intToString :: Integer -> String
intToString n = show n

-- gerarNumeroRandom :: IO
-- gerarNumeroRandom = randomR (0, 1 :: Integer)

-- replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

-- derrubarPino :: IO Integer -> [String] -> [String]
-- derrubarPino _ [] = []
-- derrubarPino n (p:ps) = do
--   number <- n
--   return $ splitAt i-1 list in ys ++ "X" ++ tail zs


-- resultadoJogada :: IO Integer -> [String] -> IO()
-- resultadoJogada n pinos = do
--   if (n <= 0) then do
--     print pinos
--   else do
--     resultadoJogada (n-1) (derrubarPino gerarNumeroRandom pinos)

-- gerarJogada :: Integer
-- gerarJogada = randomRIO (1, 10)

comecarJogo :: [String] -> Integer -> IO ()
comecarJogo pinos rodada = do
  if (rodada > 10) then do
    putStrLn("Fim de jogo!")
  else do
    putStrLn("Essa é a rodada " ++ (intToString rodada))
    putStrLn("Digite algo para começar!")
    jogada <- getLine
    result <- getStdRandom $ randomR (0, 10 :: Integer)
    if (result >= 5) then print "aaaa" else print "sadasd"
    print result
    comecarJogo pinos (rodada+1)

main :: IO ()
main = do
  putStrLn "Boliche Arcade"
  putStrLn "Digite seu nome:"
  nome <- getLine
  putStrLn ("Olá " ++ nome ++ ", vamos começar!")
  putStrLn "Iniciando o jogo..."
  putStrLn "Seus pinos: "
  print iniciarPinos
  comecarJogo iniciarPinos 1


