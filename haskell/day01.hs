import System.Environment ( getArgs )
import Data.List ( sort )

main :: IO ()
main = do
        args <- getArgs
        content <- readFile (head args)
        print (partOne content)
        print (partTwo content)

extractEven :: [String] -> [Int]
extractEven [] = []
extractEven (x:xs) = read x : extractEven (drop 1 xs)

extractOdd :: [String] -> [Int]
extractOdd xs = extractEven (drop 1 xs)

partOne :: String -> Int
partOne input =
        sum (zipWith (\x y -> abs (x - y))
                (sort (extractEven (words input)))
                (sort (extractOdd  (words input))))

count :: Int -> [Int] -> Int
count i xs = length (filter (== i) xs)

partTwo :: String -> Int
partTwo input = 
        sum (map (\x -> x * count x (extractOdd (words input)))
                (extractEven (words input)))