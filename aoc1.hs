

depth :: [Int] -> Int
depth []       = 0
depth [x]      = 0
depth (x:y:xs) = case compare x y of
    LT -> 1 + depth (y:xs)
    _ -> depth (y:xs)


depth2 :: [Int] -> Int
depth2 []        = 0
depth2 [x]       = 0
depth2 (x:[y])   = 0
depth2 (x:y:[z]) = 0
depth2 (x:xs)  = case compare (sum (x : take 2 xs)) (sum (take 3 xs)) of
    LT -> 1 + depth2 xs
    _  -> depth2 xs


program :: ([Int] -> Int) -> IO ()
program d = do
    x <- readFile "C:\\HaskellPrograms\\AdventOfCode2020\\aoc1.txt"
    let nums = map read (lines x)
    let result = d nums
    putStrLn $ show result



part1 :: IO ()
part1 = program depth

part2 :: IO ()
part2 = program depth2