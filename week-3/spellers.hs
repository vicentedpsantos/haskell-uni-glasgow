{-# LANGUAGE OverloadedStrings #-}

speller :: [String] -> String
speller [] = []
speller (head:tail) = buildSentence [head] (Prelude.length tail) ++ speller tail

buildSentence :: [String] -> Int -> String
buildSentence [] _ = ""
buildSentence [string] remaining
  | remaining == 0 = (Prelude.take 1 string) ++ " is for " ++ string
  | otherwise = (Prelude.take 1 string) ++ " is for " ++ string ++ ", and "
