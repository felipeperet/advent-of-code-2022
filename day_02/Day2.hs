module Day2 where

data Option = Rock | Paper | Scissors
    deriving (Show, Eq)

play :: Option -> Option -> Int
play Rock Rock = 4
play Rock Paper = 8
play Rock Scissors = 3
play Paper Rock = 1
play Paper Paper = 5
play Paper Scissors = 9
play Scissors Rock = 7
play Scissors Paper = 2
play Scissors Scissors = 6

strategy :: (String, String) -> (Option, Option)
strategy ("A", "X") = (Rock, Scissors)
strategy ("A", "Y") = (Rock, Rock)
strategy ("A", "Z") = (Rock, Paper)
strategy ("B", "X") = (Paper, Rock)
strategy ("B", "Y") = (Paper, Paper)
strategy ("B", "Z") = (Paper, Scissors)
strategy ("C", "X") = (Scissors, Paper)
strategy ("C", "Y") = (Scissors, Scissors)
strategy ("C", "Z") = (Scissors, Rock)
strategy _ = error "Invalid option."

parse :: String -> Option
parse c
    | c == "A" || c == "X" = Rock
    | c == "B" || c == "Y" = Paper
    | c == "C" || c == "Z" = Scissors
    | otherwise = error "Invalid option."

part1 :: IO Int
part1 = do
    sum . map ((\[x, y] -> play x y) . map parse . words) . lines
        <$> readFile
            "input.txt"

part2 :: IO Int
part2 = do
    sum
        . map (uncurry play . (\[x, y] -> strategy (x, y)) . words)
        . lines
        <$> readFile "input.txt"
