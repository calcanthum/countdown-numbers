import System.Random
import Control.Monad
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Debug.Trace
import Data.Ratio ((%))

fisherYatesShuffle :: RandomGen g => [a] -> g -> [a]
fisherYatesShuffle lst gen = go (length lst - 1) lst gen
  where
    go 0 l _ = l
    go n l g = 
      let (k, newGen) = randomR (0, n) g
          (lead, x:xs) = splitAt k l
      in x : go (n - 1) (lead ++ xs) newGen

generateGame :: Int -> StdGen -> (Int, [Int])
generateGame largeCount gen
  | largeCount < 0 || largeCount > 6 = error "Invalid number of large numbers"
  | otherwise = (targetNumber, gameNumbers)
  where
    largeNumbers = take largeCount $ fisherYatesShuffle [25, 50, 75, 100] gen
    smallNumbers = take (6 - largeCount) $ fisherYatesShuffle [1..10] gen
    gameNumbers = largeNumbers ++ smallNumbers
    (targetNumber, _) = randomR (100, 999) gen

evaluateRPN :: String -> [Int] -> Either String Rational
evaluateRPN expression gameNumbers = trace ("Initial expression: " ++ expression) $ evaluateRPN' (words expression) []
  where
    evaluateRPN' [] [result] = trace "Final result: " $ Right result
    evaluateRPN' [] _ = Left "Invalid RPN expression: Incorrect number of elements remaining."
    evaluateRPN' (token:tokens) stack =
      trace ("Current token: " ++ token ++ ", Stack: " ++ show stack) $
      case parseToken token of
        Left err -> Left err
        Right parsedToken ->
          case parsedToken of
            Number n -> processNumber n tokens stack
            Operator op -> processOperator op tokens stack

    parseToken :: String -> Either String Token
    parseToken tkn
      | tkn `elem` ["+", "-", "*", "/"] = Right $ Operator tkn
      | otherwise = case readMaybe (filter (not . isSpace) tkn) :: Maybe Int of
          Just num -> Right $ Number num
          Nothing -> Left $ "Invalid token in expression: '" ++ tkn ++ "' is not a number."

    processNumber :: Int -> [String] -> [Rational] -> Either String Rational
    processNumber number tokens stack
      | number `notElem` gameNumbers = Left $ "Invalid expression: Number " ++ show number ++ " is not in this round."
      | count number (mapMaybe readNumber (words expression)) > count number gameNumbers = 
          Left $ "Invalid expression: Number " ++ show number ++ " used more times than it appears in the game."
      | otherwise = evaluateRPN' tokens ((fromIntegral number) % 1 : stack)

    processOperator :: String -> [String] -> [Rational] -> Either String Rational
    processOperator op tokens stack
      | length stack < 2 = Left "Invalid RPN expression: not enough operands."
      | otherwise =
          let (b:a:rest) = stack
              operation = case op of
                            "+" -> (+)
                            "-" -> (-)
                            "*" -> (*)
                            "/" -> (/)
          in evaluateRPN' tokens (operation a b : rest)

    count :: Int -> [Int] -> Int
    count x lst = length . filter (==x) $ lst

    readNumber :: String -> Maybe Int
    readNumber str = readMaybe $ filter (not . isSpace) str

data Token = Number Int | Operator String

calculatePoints :: Int -> Rational -> Int
calculatePoints target result
  | difference == 0 = 10
  | difference <= 5 = 7
  | difference <= 10 = 5
  | otherwise = 0
  where difference = abs (target - (round result :: Int))

main :: IO ()
main = do
  putStrLn "Welcome to the Numbers Game!"
  gen <- newStdGen
  gameLoop gen

gameLoop :: StdGen -> IO ()
gameLoop gen = do
  putStrLn "How many large numbers?"
  largeCountInput <- getLine
  case readMaybe largeCountInput of
    Just largeCount -> 
      if largeCount < 0 || largeCount > 6 then
        putStrLn "Invalid input. Please enter a number between 0 and 6." >> gameLoop gen
      else do
        let (targetNumber, gameNumbers) = generateGame largeCount gen
        putStrLn $ "Target Number: " ++ show targetNumber
        putStrLn $ "Game Numbers: " ++ show gameNumbers
        askForSolution targetNumber gameNumbers
    Nothing -> putStrLn "Invalid input. Please enter a valid number." >> gameLoop gen

askForSolution :: Int -> [Int] -> IO ()
askForSolution targetNumber gameNumbers = do
  putStrLn "Enter your solution in Reverse Polish Notation:"
  userExpression <- getLine
  case evaluateRPN userExpression gameNumbers of
    Right result -> do
      putStrLn $ "Submitted solution: " ++ show result
      let points = calculatePoints targetNumber result
      putStrLn $ "You scored " ++ show points ++ " points!"
    Left error -> putStrLn error >> askForSolution targetNumber gameNumbers
