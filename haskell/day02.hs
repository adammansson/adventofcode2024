import System.Environment ( getArgs )

main :: IO ()
main = do
        args <- getArgs
        content <- readFile (head args)
        print (partOne content)
        print (partTwo content)

isAll :: (Int -> Int -> Bool) -> [Int] -> Bool
isAll _ [] = True
isAll _ [x] = True
isAll f (x : xs) = f x (head xs) && isAll f xs

isIncreasing :: [Int] -> Bool
isIncreasing = isAll (<)

isDecreasing :: [Int] -> Bool
isDecreasing = isAll (>)

isCorrectDifference :: [Int] -> Bool
isCorrectDifference = isAll (\x y -> abs (x - y) `elem` [1..3])

isSafe :: [Int] -> Bool
isSafe report =
        (isIncreasing report  ||
         isDecreasing report) &&
        isCorrectDifference report

count :: [Bool] -> Int
count xs = length (filter id xs)

partOne :: String -> Int
partOne input =
        count (map (isSafe . (map read . words)) (lines input))

removeAt :: Int -> [Int] -> [Int]
removeAt i xs =
        case splitAt i xs of
                (xs, ys) -> xs ++ drop 1 ys

isSafeDampenedRecursive :: Int -> [Int] -> Bool
isSafeDampenedRecursive (-1) _ = False
isSafeDampenedRecursive i xs =
        isSafe (removeAt i xs) ||
        isSafeDampenedRecursive (i - 1) xs

isSafeDampened :: [Int] -> Bool
isSafeDampened report =
        isSafe report ||
        isSafeDampenedRecursive (length report) report

partTwo :: String -> Int
partTwo input =
        count (map (isSafeDampened . (map read . words)) (lines input))
