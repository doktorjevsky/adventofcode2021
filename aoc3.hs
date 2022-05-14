
import Data.Bits (xor)


gamma :: [String] -> Int -> String 
gamma ("":xs) _ = ""
gamma xs threshold = let (cnt, xs') = countOnes xs in case compare cnt threshold of
    GT -> '1' : gamma xs' threshold
    EQ -> '1' : gamma xs' threshold
    _  -> '0' : gamma xs' threshold


oxygen :: Int -> [String] -> String
oxygen _ [x] = x 
oxygen i xs  = oxygen (i+1) (filterAt i xs compare '1')

co2 :: Int -> [String] -> String 
co2 _ [x] = x 
co2 i xs  = co2 (i + 1) (filterAt i xs (flip compare) '0')

filterAt ::  Int -> [String] -> (Int -> Int -> Ordering) -> Char -> [String]
filterAt i xs f c = filter (\s -> s!!i == getChAt c i xs f) xs

getChAt :: Char -> Int -> [String] -> (Int -> Int -> Ordering) -> Char 
getChAt c i xs f = let (z, o) = countBothAt i xs (0, 0) in case f z o of
    GT -> '0'
    LT  -> '1'
    EQ -> c

countBothAt :: Int -> [String] -> (Int, Int) -> (Int, Int)
countBothAt i [] t = t 
countBothAt i (x:xs) (z, o) = case x!!i of
    '1' -> countBothAt i xs (z, o + 1)
    '0' -> countBothAt i xs (z + 1, o)
    _   -> countBothAt i xs (z, o)


bitAt :: Eq a => Int -> a -> [a] -> Bool 
bitAt n x xs = xs !! n == x 


countOnes :: [String] -> (Int, [String])
countOnes ("":xs) = (0, "":xs)
countOnes xs = (sum [1 | x <- xs, head x == '1'], map tail xs)

binaryToDec :: String -> Int
binaryToDec xs = sum $ map (\(n, e) -> read [n] * intExp 2 e) (zip (reverse xs) [0,1..])

intExp :: Int -> Int -> Int 
intExp n 0 = 1
intExp n e = case even e of
    True  -> intExp (n*n) (div e 2)
    False -> n * intExp n (e - 1)

getBitCnt :: Int -> Int 
getBitCnt 0 = 0
getBitCnt n = 1 + getBitCnt (div n 2)

program :: IO ()
program = do 
    myData <- readFile "C:\\HaskellPrograms\\AdventOfCode2020\\aoc3.txt"
    let strNumbers = lines myData
    let gRay = binaryToDec $ gamma strNumbers (div (length strNumbers) 2)
    let eRay = xor gRay (binaryToDec (take (getBitCnt gRay) ['1','1'..]))
    let oxy = binaryToDec $ oxygen 0 strNumbers
    let carbon = binaryToDec $ co2 0 strNumbers
    putStrLn "Part 1:"
    putStrLn $ show $ gRay * eRay 

    putStrLn "Part 2:"
    putStrLn $ show $ oxy * carbon