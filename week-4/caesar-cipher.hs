import Data.Char

shouldCipher :: Char -> Bool
shouldCipher c = isLetter(c) && isAscii(c)

cipher :: Int -> [Char] -> [Char]
cipher shift plaintext = map ( betterCipherChar shift) plaintext

decipher :: Int -> [Char] -> [Char]
decipher shift cipheredText = cipher(-shift) cipheredText

wrapAround shift c
  | isLower(c) && ord(c) + shift > ord 'z' = True
  | isUpper(c) && ord(c) + shift > ord 'Z' = True
  | otherwise = False

betterCipherChar :: Int -> Char -> Char
betterCipherChar shift c
  | shouldCipher c = chr(ord(c) + adjustedShift)
  | otherwise      = c
  where adjustedShift = let shift' = shift `mod` 26
                        in if (wrapAround shift' c)
                           then shift'-26
                           else shift'
