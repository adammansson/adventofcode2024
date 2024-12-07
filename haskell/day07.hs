import System.Environment ( getArgs )

main :: IO ()
main = do
        args <- getArgs
        content <- readFile (head args)
        print (partOne content)
        print (partTwo content)

parseLine :: String -> (Integer, [Integer])
parseLine line =
        (read (filter (/= ':') (head (words line))), map read (tail (words line)))

checkEquation :: (Integer, [Integer]) -> Bool
checkEquation (lhs, [x]) = lhs == x
checkEquation (lhs, x0:x1:xs) = 
        checkEquation (lhs, x0 + x1 : xs) ||
        checkEquation (lhs, x0 * x1 : xs)

checkEquationWithConcat :: (Integer, [Integer]) -> Bool
checkEquationWithConcat (lhs, [x]) = lhs == x
checkEquationWithConcat (lhs, x0:x1:xs) = 
        checkEquationWithConcat (lhs, x0 + x1 : xs) ||
        checkEquationWithConcat (lhs, x0 * x1 : xs) ||
        checkEquationWithConcat (lhs, read (show x0 ++ show x1) : xs)

calibrationResult :: ((Integer, [Integer]) -> Bool) -> String -> Integer
calibrationResult f input =
        sum (map fst (filter f (map parseLine (lines input))))

partOne :: String -> Integer
partOne =
        calibrationResult checkEquation 

partTwo :: String -> Integer
partTwo =
        calibrationResult checkEquationWithConcat
