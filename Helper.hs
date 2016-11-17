import Data.List
import Sowpods

type Board = [[Maybe Char]]



autoResize :: Board -> Board
autoResize bd = bd3
                where bd3           = transpose (buildResize bd2 (west,east))
                      (west, east)  = autoResHelper bd2 (7,7)
                      bd2           = transpose (buildResize bd (north,south))
                      (north,south) = autoResHelper bd (7,7)
                 
--autoResize = Bram.autoResize



buildResize :: Board -> (Int,Int) -> Board
buildResize bd (top,bottom) = [ (replicate top Nothing) ++ b ++ (replicate bottom Nothing) | b <- bd]

--helper function which returns the smallest number of empty tiles before any letter from (top,bottom)
autoResHelper :: Board -> (Int,Int) -> (Int,Int)
autoResHelper []     (top,bottom) = ((7-top),(7-bottom))
autoResHelper (x:xs) (top,bottom) = autoResHelper xs (newTop,newBottom)
                                       where newTop    = minimum ([(countNothing x 0)]++[top])
                                             newBottom = minimum ([(countNothing (reverse x) 0)]++[bottom])


--count letter occurance from top of a single line
countNothing :: [Maybe Char] -> Int -> Int
countNothing []     n        = n
countNothing (Nothing:xs)  n = countNothing xs (n+1)
countNothing ((Just _):xs) n = n


bd = [[Just 't'],[Just 'e'],[Just 's'],[Just 't']]


-- Positions, as (x, y). As you go down, y increases; as you go right, x
-- increases:
type Pos = (Int, Int)

-- An orientation can be horizontal (H) or vertical (V):
data Orient = H | V deriving (Show, Eq, Read)

-- A word position is a start position together with an orientation:
type WordPos = (Pos, Orient)

-- A move is a word (represented as a String) together with a position:
type Move = (String, WordPos)

-- A score is represented as an Int:
type Score = Int


-- Exercise, medium/hard. Given a board and a move that is to be executed on
-- that board, put the move on the board and return the resulting board. You
-- can assume that the given move is valid on the given board.

writeMove :: Move -> Board -> Board
writeMove (wrd,((x,y),V)) bd =  (take y bd) ++ [((take y (bd !! x)) ++ (strToCharList wrd) ++ (drop (y+(length wrd)) (bd !! x)))] ++ (drop (y+1) bd) 
writeMove (wrd,((x,y),H)) bd =  transpose ((take x bd2) ++ [((take x (bd2 !! y)) ++ (strToCharList wrd) ++ (drop (x+(length wrd)) (bd2 !! y)))] ++ (drop (x+1) bd2))
                                where bd2 = transpose bd

strToCharList :: String -> [Maybe Char]
strToCharList xs = [Just x | x <- xs]


allWords1 :: [String] -> Char -> [String]
allWords1 dt n = filter (\xs -> n `elem` xs) dt

--	movetest = writeMove  (autoResize bd)


-- Exercise, hard.
--
-- Say that we have the following row on our board:
--
--     X - - - - - - - E - - - - - - X
--         E L S E W H E R E
-- We're wondering which words would fit there, overlapping with the
-- E. The word 'elsewhere' has four Es, and fits in two ways, namely
-- with the middle two E's. (As always in SCRABOL, when you play a
-- word they must overlap with exactly one tile on the board, so
-- 'xenophobe' would not fit here.)
--
-- Given an "intersection letter" (in this case E), and the number of
-- free spaces around it, find all the words that would fit in such a
-- space. Also, give all the positions where that letter is anchored;
-- in the example, 'elsewhere' can be anchored at positions 3 or 6.
--
-- In this example, the number of free spaces is 6 and 5,
-- respectively, because we must leave some space around the Xes.
--
-- allWords2 dict 'e' 6 5 = [..., ("elsewhere", 3), ("elsewhere", 6"), ...]
--
-- allWords2 dict 'x' 1 2 = [("ax", 2), ("axe", 2), ("axed", 2),
--    ("axes", 2), ("axis", 2), ("axle", 2), ("axon", 2), ("ex", 2), ("exam",
--    2), ("exec", 2), ("exes", 2), ("exit", 2), ("expo", 2), ("ox", 2),
--    ("oxen", 2), ("x", 1)]
--
-- (You may give the words in a different order.)

type Dict = [String]

allWords2 :: Dict -> Char -> Int -> Int -> [(String, Int)]
allWords2 dict ch low up = concat [ fitList (findPosition str ch []) str low up | str <- (allWords1 dict ch) ]


--find all positions of particular character in the single words
findPosition :: String -> Char -> [Int] -> [Int]
findPosition [] _  ls = normalise ls 0
findPosition xs ch ls = case elemIndex ch xs of
                             Nothing -> normalise ls 0
                             Just x  -> findPosition (drop (x+1) xs) ch (ls++[x])

--normalise the list of positions
normalise :: [Int] -> Int -> [Int]
normalise []     _ = []
normalise (x:xs) n = (x+n) : normalise xs (x+n+1)

--find all the posibilities to fit the particular word into the available space
fitList :: [Int] -> String -> Int -> Int ->  [(String, Int)]
fitList [] _ _ _  = []
fitList (x:xs) str low up | (low >= x) && (up >= ((length str)-x-1)) = (str,x) : fitList xs str low up 
                          | otherwise                                = fitList xs str low up
                          




--allWords2 = Bram.allWords2