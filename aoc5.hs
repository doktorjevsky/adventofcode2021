{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
import qualified Data.Map as M
import Data.Maybe

data Line = L (Int, Int) (Int, Int) deriving (Show, Eq, Ord)

parsePoints :: FilePath -> IO ()
parsePoints fp = do 
    s <- readFile fp 
    let string = words $ filter (\c -> c /= '-' && c /= '>') s
  --  print string
    let lines = getStraightLines string ++ getDiagonalLines string
    print $ calculateOverLap (getLineMap lines M.empty)
   -- return (lines)

getLines :: [String] -> [Line]
getLines [] = []
getLines xs = L (parseTup t) (parseTup t') : getLines (drop 2 xs) 
    where
        parseTup :: String -> (Int, Int)
        parseTup s = (read $ takeWhile (/= ',') s, read $ drop 1 $ dropWhile (/= ',') s)
        xs' = take 2 xs
        t   = head xs' 
        t'  = head $ tail xs'

getStraightLines :: [String] -> [Line]
getStraightLines s = filter isStraightLine $ getLines s

getDiagonalLines :: [String] -> [Line]
getDiagonalLines s = filter (not . isStraightLine) $ getLines s

isStraightLine :: Line -> Bool 
isStraightLine (L (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

calculatePoints :: Line -> [(Int, Int)]
calculatePoints (L (x1, y1) (x2, y2)) = zip xs ys 
    where
        f :: Int -> Int -> [Int]
        f a b = if a < b then [a .. b] else [a, a-1 .. b]
        xs = if x1 == x2 then [x1, x1 ..] else f x1 x2
        ys = if y1 == y2 then [y1, y1 ..] else f y1 y2

getLineMap :: [Line] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
getLineMap [] m     = m 
getLineMap (l:ls) m = getLineMap ls m'
    where
        m' = foldr (\tup m' -> if M.member tup m' then M.adjust ( + 1) tup m' else M.insert tup 1 m') m (calculatePoints l)

calculateOverLap :: M.Map (Int, Int) Int -> Int 
calculateOverLap m = foldr (\k z -> if fromJust (M.lookup k m) > 1 then 1 + z else z) 0 (M.keys m)