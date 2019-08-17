import System.Random (randomIO)
import System.IO
import Data.List
import Data.Char
--A game of hangman by Yusdel Lorenzo 2019
myPrint :: String -> String
myPrint word = intersperse ' ' [if c `elem` ['A'..'Z'] then '_' else c | c <- word]

guess :: String -> Char -> IO ()
guess word letter
  | letter `elem` word = game [if letter == c then toLower letter else c | c <- word]
  | otherwise = game word

game :: String -> IO ()
game word
  | word == map toLower word = do
    putStrLn $ myPrint word
  | otherwise = do
    putStrLn $ myPrint word
    userGuess <- getLine
    case userGuess of
      c:_ -> guess word (toUpper $ head userGuess)
      _ -> game word

main :: IO ()
main = do
  contents <- readFile "words.txt"
  let words' = (lines contents)
  let wordcount = length words'
  randomNum <- randomIO
  let randomWord = words' !! (randomNum `mod` wordcount)
  case randomWord of
    c:_ -> game (map toUpper randomWord)
    _ -> main
