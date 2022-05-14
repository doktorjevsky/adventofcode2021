import qualified Data.Map as M 

emptyState :: M.Map Int Int
emptyState = foldr (\x m -> M.insert x 0 m) M.empty [0..8]

initialState :: [Int] -> M.Map Int Int 
initialState = foldr (\x m -> M.adjust (+ 1) x m) emptyState

-- 0 becomes 6 and gives birth to 8
newState :: M.Map Int Int -> M.Map Int Int 
newState m = foldr (\(k, v) m' -> M.adjust (+v) k m') m'' zipped 
    where 
        ks = M.keys m
        (v:vs) = M.elems m
        zipped = zip ks (vs ++ [v])
        m'' = M.adjust (+v) 6 emptyState


test :: M.Map Int Int 
test = initialState [3,4,3,1,2]

simulate :: Int -> M.Map Int Int -> Int 
simulate 0 m     = sum (M.elems m)
simulate nDays m = simulate (nDays - 1) (newState m)


realData = initialState [1,1,1,3,3,2,1,1,1,1,1,4,4,1,4,1,4,1,1,4,1,1,1,3,3,2,3,1,2,1,1,1,1,1,1,1,3,4,1,1,4,3,1,2,3,1,1,1,5,2,1,1,1,1,2,1,2,5,2,2,1,1,1,3,1,1,1,4,1,1,1,1,1,3,3,2,1,1,3,1,4,1,2,1,5,1,4,2,1,1,5,1,1,1,1,4,3,1,3,2,1,4,1,1,2,1,4,4,5,1,3,1,1,1,1,2,1,4,4,1,1,1,3,1,5,1,1,1,1,1,3,2,5,1,5,4,1,4,1,3,5,1,2,5,4,3,3,2,4,1,5,1,1,2,4,1,1,1,1,2,4,1,2,5,1,4,1,4,2,5,4,1,1,2,2,4,1,5,1,4,3,3,2,3,1,2,3,1,4,1,1,1,3,5,1,1,1,3,5,1,1,4,1,4,4,1,3,1,1,1,2,3,3,2,5,1,2,1,1,2,2,1,3,4,1,3,5,1,3,4,3,5,1,1,5,1,3,3,2,1,5,1,1,3,1,1,3,1,2,1,3,2,5,1,3,1,1,3,5,1,1,1,1,2,1,2,4,4,4,2,2,3,1,5,1,2,1,3,3,3,4,1,1,5,1,3,2,4,1,5,5,1,4,4,1,4,4,1,1,2]

part1 = simulate 80 realData

part2 = simulate 256 realData