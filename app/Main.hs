module Main where

data Color
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Pink
  deriving (Show)

data ColorState
  = ColorState Color Color Color Color
  deriving (Show)

startingSecret = ColorState Red Orange Yellow Green

charToColor :: Char -> Maybe Color
charToColor 'r' = Just Red
charToColor 'o' = Just Orange
charToColor 'y' = Just Yellow
charToColor 'g' = Just Green
charToColor 'b' = Just Blue
charToColor 'p' = Just Pink
charToColor _ = Nothing

rawToState :: String -> Maybe ColorState
rawToState [a, b, c, d] = do
  a' <- charToColor a
  b' <- charToColor b
  c' <- charToColor c
  d' <- charToColor d

  return (ColorState a' b' c' d')
rawToState _ = Nothing

promptForGuess :: IO ColorState
promptForGuess = do
  rawGuess <- getLine
  let maybeState = rawToState rawGuess
  case maybeState of
    Nothing -> do
      putStrLn "please try again"
      promptForGuess

    Just s -> return s

main = do
  putStrLn "Make a guess: "
  guess <- promptForGuess
  print guess
