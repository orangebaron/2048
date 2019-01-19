import System.Random

rotate :: [[a]] -> [[a]]
rotate [] = []
rotate ([]:_) = []
rotate x = (map head x) : (rotate $ map tail x)

mutate :: (Bool,Bool) -> [[a]] -> [[a]]
mutate (False,a) = if (a) then (map reverse) else id
mutate (True,a) = (mutate (False,a)) . rotate

unmutate :: (Bool,Bool) -> [[a]] -> [[a]]
unmutate a@(False,_) = mutate a
unmutate (True,a) = rotate . (mutate (False,a))

incNonNeg1 :: Int -> Int
incNonNeg1 (-1) = -1
incNonNeg1 x = x + 1

setElem :: [a] -> Int -> a -> [a]
setElem [] _ _ = []
setElem (a:t) 0 x = x:t
setElem (a:t) n x = a:(setElem t (n-1) x)

nextNonZero :: [Int] -> Int
nextNonZero [] = -1
nextNonZero (0:a) = incNonNeg1 $ nextNonZero a
nextNonZero _ = 0

nextX :: Int -> [Int] -> Int
nextX _ [] = -1
nextX a (0:t) = incNonNeg1 $ nextX a t
nextX a (b:_) = if (a==b) then 0 else -1

gravity :: [Int] -> [Int]
gravity [] = []
gravity (0:t) = let next = nextNonZero t in
  if (next == -1) then (0:t) else gravity $ (t!!next):(setElem t next 0)
gravity (x:t) = let next = nextX x t in
  if (next == -1) then (x:(gravity t)) else (x+1):(gravity $ setElem t next 0)
  
doMove :: (Bool,Bool) -> [[Int]] -> [[Int]]
doMove x = (unmutate x) . (map gravity) . (mutate x)

find1D :: (Eq a) => a -> [a] -> [Int]
find1D _ [] = []
find1D a (b:rest) = (if (a==b) then [0] else [])++(map (+1) $ find1D a rest)

tupleIfy :: a -> [b] -> [(a,b)]
tupleIfy x = map (\y -> (x,y))

make2DTuples :: [[Int]] -> [(Int,Int)]
make2DTuples locs = foldl (\tupleLocs index -> tupleLocs++(map (tupleIfy index) locs!!index)) [] [0..(length locs-1)]

getAllLocsEqualTo :: (Eq a) => a -> [[a]] -> [(Int,Int)]
getAllLocsEqualTo a = make2DTuples . (map $ find1D a)

randomSpot :: [[Int]] -> IO (Int,Int)
randomSpot l = (randomRIO (0,(length locs - 1))) >>= (\r -> return $ locs !! r) where locs = getAllLocsEqualTo 0 l

fillRandomSpot :: [[Int]] -> IO [[Int]]
fillRandomSpot l = (randomSpot l) >>= (\loc -> return $ setElem l (fst loc) (setElem (l!!(fst loc)) (snd loc) 1))

takeTurn :: (Bool,Bool) -> [[Int]] -> IO [[Int]]
takeTurn x y = fillRandomSpot $ doMove x y

appendWithDelim :: [a] -> [a] -> [a] -> [a]
appendWithDelim d a b = a ++ d ++ b

foldWithDelim :: [a] -> [a] -> [[a]] -> [a]
foldWithDelim a start = tail . (foldl (appendWithDelim a) start)

numToString :: Int -> Int -> String
numToString mostDigits n
  | n == 1 = replicate mostDigits ' '
  | otherwise = let s = show n in (replicate (mostDigits-(length s)) ' ') ++ s

lineToString :: Int -> [Int] -> String
lineToString mostDigits = (foldWithDelim "|" "") . (map $ numToString mostDigits)

mostNumDigits :: [Int] -> Int
mostNumDigits = maximum . (map (length . show))

matrixToString :: [[Int]] -> String
matrixToString l = foldWithDelim "\n" "" $ map (lineToString $ mostNumDigits $ foldl (++) [] l) l

boardToString :: [[Int]] -> String
boardToString l = matrixToString $ map (map (2^)) l

printBoard :: [[Int]] -> IO ()
printBoard = putStrLn . boardToString

inputMove :: IO (Bool,Bool)
inputMove = getChar >>= (\x -> case x of
  'h' -> return (False,False)
  'l' -> return (False,True)
  'k' -> return (True,False)
  'j' -> return (True,True)
  'a' -> return (False,False)
  'd' -> return (False,True)
  'w' -> return (True,False)
  's' -> return (True,True)
  otherwise -> inputMove)

fullGame :: [[Int]] -> IO ()
fullGame b = do
  printBoard b
  move <- inputMove
  newB <- takeTurn move b
  fullGame newB

main = fullGame [[1,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]
