import System.Random
import System.IO.Unsafe

check :: String -> String -> Char -> (Bool, String)
check word display c
  = (c `elem` word, [if x == c
    then x
    else y | (x, y) <- zip word display])

turn :: String -> String -> Int -> IO ()
turn word display 0 = putStrLn "You Lose"
turn word display n =
  do if word == display
      then putStrLn "You win!"
      else mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStr "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

starman :: Int -> IO ()
starman numberOfChances =
  do
    let availableWords = ["fish", "butter", "pepper", "berries", "stonks"]
    randomWord <- getStdRandom (randomR (0, length availableWords))
    let word' = availableWords !! randomWord
    turn word' ['-' | x <- word'] numberOfChances
