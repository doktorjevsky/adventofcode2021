

totalCost :: [Int] -> Int -> (Int -> Int) -> Int 
totalCost crabPositions toPosition costF = foldr (\cp z -> z + costF (abs (toPosition - cp))) 0 crabPositions

allCosts :: [Int] -> (Int -> Int) -> [Int]
allCosts crabs costF = allCosts' crabs 0 (length crabs) (minimum crabs) costF
    where
        allCosts' :: [Int] -> Int -> Int -> Int -> (Int -> Int) -> [Int]
        allCosts' crabs i end startVal costF = if i < end then totalCost crabs startVal costF : allCosts' crabs (i+1) end (startVal + 1) costF else []

part1 :: Int -> Int 
part1 x = x 

part2 :: Int -> Int 
part2 x = div (x * (x + 1)) 2

minCost :: [Int] -> (Int -> Int) -> Int 
minCost f = minimum . allCosts f

crabDataTest :: [Int]
crabDataTest = [16,1,2,0,4,2,7,1,2,14]

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c xs = takeWhile (/= c) xs : splitBy c (drop 1 (dropWhile (/= c) xs))

bigTest :: IO ()
bigTest = do 
    crabPos <- readFile "aoc7.txt"
    let out = minCost (map read (splitBy ',' crabPos))
    putStrLn $ "Part 1: " ++ show (out part1)
    putStrLn $ "Part 2: " ++ show (out part2)