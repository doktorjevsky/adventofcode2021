

data Board = R [Row] 

type Row = [(Int, Bool)]

instance Show Board where
    show (R [])     = "\n"
    show (R (r:rs)) = show r ++ "\n" ++ show (R rs)


separateByCh :: Char -> String -> [String]
separateByCh _ "" = []
separateByCh c s  = takeWhile (/= c) s : separateByCh c s'
   where 
       s' = drop 1 (dropWhile (/= c) s)

myRead :: String -> Int 
myRead s = read s 

toRows :: [[Int]] -> [Row]
toRows [] = []
toRows (x:xs) = map (\i -> (i, False)) x : toRows xs

toBoard :: [String] -> Board
toBoard xs =  R $ toRows $ map (\s -> map myRead (words s)) $ take 5 $ filter (/= "") xs

getBoards :: [String] -> [Board]
getBoards [] = []
getBoards xs = toBoard xs : getBoards (drop 6 xs)

process :: IO ([Int], [Board])
process = do 
    myData <- readFile "C:\\HaskellPrograms\\AdventOfCode2020\\aoc4.txt" 
    let numbers = map (\s -> myRead s) $ separateByCh ',' $ head $ words myData
    let boardData = getBoards $ tail $ lines myData

    return (numbers, boardData)

checkRows :: Board -> (Bool, Row)
checkRows (R []) = (False, [])
checkRows (R (r:rs)) = if and (map (\(_, b) -> b) r) then (True, r) else checkRows (R rs)

checkCols :: Board -> (Bool, Row)
checkCols (R []) = (False, [])
checkCols (R rs) = if and (map (\(_, b) -> b) col) then (True, col) else checkCols (R rs')
    where
        col = concat (map (take 1) rs)
        rs' = [drop 1 x | x <- rs]

tickBox :: Int -> [Board] -> [Board]
tickBox n [] = []
tickBox n (b:bs) = R (tickBox' n b) : tickBox n bs
    where
        tickBox' :: Int -> Board -> [Row]
        tickBox' n (R []) = []
        tickBox' n (R (r:rs)) = map (\(i, b) -> if i == n then (i, True) else (i, b)) r : tickBox' n (R rs)

part1 :: [Int] -> [Board] -> Int -> (Board, Int)
part1 [] _ _= (R [], 0)
part1 (x:xs) bs last = case filter (\b -> fst (checkRows b)) bs of
    [z] ->  (z, last)
    _  -> case filter (\b -> fst (checkCols b)) bs of
        [y] -> (y, last) 
        _  -> part1 xs (tickBox x bs) x


part2 :: [Int] -> [Board] -> Int -> [Board] -> (Board, Int)
part2 [] bs last prev = (head prev, last)
part2 (x:xs) bs last prev = case compare (length bs) 1 of
    LT -> (head prev, last)
    EQ -> (head bs, last)
    GT -> part2 xs bs' x (tickBox x bs)
        where
            bs' = [b | b <- tickBox x bs, (not . fst . checkCols) b || (not . fst . checkRows) b]
       



sumOfUnmarked :: Board -> Int
sumOfUnmarked (R rs) = sum (map (\(i,_) -> i) (filter (\(i, b) -> not b) (concat rs)))

main :: IO ()
main = do
    (nums, boards) <- process
    let (R result, winNum) = part1 nums boards 0
    let (R result2, winNum2) = part2 nums boards 0 []

    putStrLn $ show $ length boards
    putStrLn "Part 1: "
    putStrLn $ show $ sumOfUnmarked (R result) * winNum

    putStrLn "Part 2: "

    putStrLn $ show $ R result2
    putStrLn $ show winNum2
    putStrLn $ show $ winNum2 * sumOfUnmarked (R result2)

