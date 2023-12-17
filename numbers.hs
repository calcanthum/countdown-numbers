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
evaluateRPN expression gameNumbers = evaluateRPN' (words expression) []
  where
    evaluateRPN' [] [result] = Right result
    evaluateRPN' [] _ = Left "Invalid RPN expression: Incorrect number of elements remaining."
    evaluateRPN' (token:tokens) stack = trace ("Debug: Processing token: " ++ token ++ ", Stack: " ++ show stack) $
      case token `elem` ["+", "-", "*", "/"] of
        True ->
          if length stack < 2 then
            Left "Invalid RPN expression: not enough operands."
          else
            let (b:a:rest) = stack
                op = case token of
                        "+" -> (+)
                        "-" -> (-)
                        "*" -> (*)
                        "/" -> (/)
                in evaluateRPN' tokens (op a b : rest)
        False ->
          case readMaybe (filter (not . isSpace) token) :: Maybe Int of
            Just number -> 
              if number `notElem` gameNumbers then
                Left $ "Invalid expression: Number " ++ show number ++ " is not in this round."
              else if count number (map (read . filter (not . isSpace)) (words expression) :: [Int]) > count number gameNumbers then
                Left $ "Invalid expression: Number " ++ show number ++ " used more times than it appears in the game."
              else
                evaluateRPN' tokens ((fromIntegral number) % 1 : stack)
            Nothing -> 
                let filteredToken = filter (not . isSpace) token
                in trace ("Debug: Failed to parse token as number: " ++ token ++ ", Filtered Token: " ++ filteredToken) 
                    Left $ "Invalid token in expression: '" ++ token ++ "' is not a number."

    count x lst = length . filter (==x) $ lst


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
      putStrLn $ "Result of Expression: " ++ show result
      let points = calculatePoints targetNumber result
      putStrLn $ "You scored " ++ show points ++ " points!"
    Left error -> putStrLn error >> askForSolution targetNumber gameNumbers
