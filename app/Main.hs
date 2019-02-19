module Main where

data Color
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Pink
  deriving (Show, Eq)

data ColorState
  = ColorState Color Color Color Color
  deriving (Show, Eq)

startingSecret = ColorState Red Orange Yellow Orange

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
-- do is monad chain operation:
-- charToColor a >>= \a' -> (charToColor b >>= \b' -> (charToColor c >>= \c' -> (....)))
-- Maybe Color -> (Color -> Maybe ColorState) -> Maybe ColorState


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

    (Just s) -> return s

checkGuess :: Int -> IO ()
checkGuess n = do
  putStrLn ("Make a guess: " ++ (show (11-n)) ++ "/10")
  guess <- promptForGuess
  print guess
  let hintState = compareStates guess startingSecret
  print hintState
  if hintState == [Black, Black, Black, Black] 
    then putStrLn "You won!" >> return () 
    else if n == 1 
      then putStrLn "You lost!" >> return () 
      else checkGuess (n - 1)
   

main = checkGuess 10

data Hint
  = Black
  | White
  deriving(Show,Eq)

type HintState = [Hint]  

compareStates :: ColorState -> (ColorState -> HintState)
compareStates (ColorState a b c d) (ColorState a' b' c' d') =
  let boolSame = map (\(x,y) -> (x==y, (x, y))) (zip [a, b, c, d] [a', b', c', d'])
      matches = map (\_ -> Black) (filter fst boolSame)
      unused = map snd (filter (not . fst) boolSame) -- (\x -> not (fst x))
      whites = map (\_ -> White) (filter (\(x,y) -> elem x (map snd unused)) unused)
  in matches ++ whites
  -- do return outputs array so use let in or do without return statement
