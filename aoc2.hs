

data Position = P Depth Vertical Aim deriving Show

type Vertical = Int
type Depth    = Int
type Aim      = Int


updatePosition :: (String, Int) -> Position -> Position
updatePosition (s, n) p@(P h v a) = case s of
    "forward" -> P (h + n*a) (v+n) a
    "down"    -> P h v (a + n)
    "up"      -> P h v (a - n)
    _         -> p

updatePositions :: Position -> ((String, Int) -> Position -> Position) -> [(String, Int)] -> Position
updatePositions p f ps = foldl (flip f) p ps

toTup :: [String] -> (String, Int)
toTup (x:[y]) = (x, read y)
toTup _ = ("", 0)

-- it's really part 2 by now : -- )
part1 :: [(String, Int)] -> Position
part1 = updatePositions (P 0 0 0) updatePosition

program :: ([(String, Int)] -> Position) -> IO ()
program f = do
    myData <- readFile "C:\\HaskellPrograms\\AdventOfCode2020\\aoc2.txt"
    let instructions = map (\s -> toTup (words s)) (lines myData) 
    let p@(P depth vert aim) = f instructions
    putStrLn $ show $ depth * vert
    putStrLn $ show p