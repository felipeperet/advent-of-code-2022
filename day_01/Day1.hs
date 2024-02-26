module Day1 where

import Data.Functor ((<&>))
import Data.List (sortBy)

countCalories :: FilePath -> IO [Integer]
countCalories fileName = do
    contents <- lines <$> readFile fileName
    let elfs :: [String] -> [[String]]
        elfs input = case break (== "") input of
            ([], _) -> []
            (xs, []) -> [xs]
            (xs, _ : ys) -> xs : elfs ys
        calories = map (map (read :: String -> Integer)) (elfs contents)
        sumCalories = map sum calories
    return sumCalories

part1 :: IO Integer
part1 = countCalories "input.txt" <&> maximum

part2 :: IO Integer
part2 = countCalories "input.txt" <&> (sum . take 3 . sortBy (flip compare))
