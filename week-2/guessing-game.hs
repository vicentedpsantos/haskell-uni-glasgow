{-# START_FILE main.hs #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics
import System.Random
import System.IO.Unsafe

data Question = 
  Question { category          :: !String
            , difficulty       :: !String
            , content          :: !String
            , correctAnswer    :: !String
            , incorrectAnswers :: [String]
            } deriving (Show, Generic)

instance FromJSON Question
instance ToJSON Question

jsonFile :: FilePath
jsonFile = "questions.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

fetchQuestions :: IO (Either String [Question])
fetchQuestions = (eitherDecode <$> getJSON) :: IO (Either String [Question])

main :: IO ()
main = do
  d <- fetchQuestions
  case d of
    Left err -> putStrLn err
    Right questions -> do
      let chosenQuestion = questions !! getRandomInt 0 30
      displayQuestion chosenQuestion
      mkGuess chosenQuestion
      print "End game"

checkAnswer :: Question -> String -> IO ()
checkAnswer question answer
  | answer == (correctAnswer question) = displayResult (True, correctAnswer question)
  | otherwise                          = displayResult (False, correctAnswer question)

mkGuess :: Question -> IO ()
mkGuess question = do
  putStrLn "  Enter your Answer: "
  answer <- getLine
  checkAnswer question answer

displayResult :: (Bool, String) -> IO ()
displayResult (hasWon, correctAnswer) =
  do if hasWon
      then putStrLn "You are correct"
      else putStrLn "Wrong."

displayQuestion :: Question -> IO ()
displayQuestion question = do
  print (content question)
  print ((incorrectAnswers question) ++ ([correctAnswer question]))

getRandomInt :: Int -> Int -> Int
getRandomInt rangeStart rangeEnd = unsafePerformIO (getStdRandom (randomR (rangeStart, rangeEnd)))
