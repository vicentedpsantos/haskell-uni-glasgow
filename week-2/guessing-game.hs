{-# START_FILE main.hs #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

data Question = 
  Question { category          :: !Text
            , difficulty       :: !Text
            , question         :: !Text
            , correctAnswer    :: !Text
            , incorrectAnswers :: [Text]
            } deriving (Show, Generic)

instance FromJSON Question
instance ToJSON Question

-- this is my attempt of fetching the questions from
-- a public API. I believe I could not do it because
-- they are coming inside a "results" object and I could
-- not find a way to parse that response
-- jsonURL :: String
-- jsonURL = "https://opentdb.com/api.php?amount=1"

-- getJSON :: IO B.ByteString
-- getJSON = simpleHttp jsonURL

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
    Right questions -> print $ displayQuestion questions

displayQuestion :: Question
displayQuestion questions = 
  questions !! unsafePerformIO (getStdRandom (randomR (0, length questions)))
