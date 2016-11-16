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