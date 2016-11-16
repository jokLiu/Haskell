import Data.List


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


--movetest = writeMove  (autoResize bd)