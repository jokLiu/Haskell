{-Chapter 2-}
--Exercise 2.
--(2^3)*4
--(2*3)+(4*5)
--2+(3*(4^5))

--Exercise 3
n :: Int
n = a `div` length xs
    where 
        a  = 10
        xs = [1,2,3,4,5]

--Exercise 4
last2 :: [a] -> a
last2 = head . reverse

--Exercise 5
init1 :: [a] -> [a]
init1 xs = take ((length xs)-1) xs

init2 :: [a] -> [a]
init2 = reverse . drop 1 . reverse

{-Chapter 3-}
--Exercise 1
--a) [Char]
--b) (Char,Char,Char)
--c) [(Bool, Char)]
--d) [[a]->[a]]

--Exercise 2
bools:: [Bool]
bools = [True, False]

nums:: [[Int]]
nums = [[1,2,3],[2],[23,12312]]

add :: Int -> Int -> Int -> Int
add a b c = a+b+c

copy:: a -> (a,a)
copy x = (x,x)

apply :: (a->b) -> a -> b
apply g x = g x

--Exercise 3
--second :: [a] -> a
second xs = head (tail xs)
--swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
--pair :: a -> b -> (a,b)
pair x y = (x,y)
--double :: Num a => a -> a
double x = x*2
--palindrome :: Eq => [a] -> Bool
palindrome xs = reverse xs == xs
--twice :: (a->a) -> a -> a 
twice f x = f (f x)

--Exercise 4
--Xhecked all the answers on GHCi, and everything worked

--Exercise 5


{-Chpater 4-}
--Exercise 1
halve :: [a] -> ([a],[a])
halve xs = ((take len xs),(drop len xs))
           where len = (length xs) `div` 2

--Exercise 2
--a
third1:: [a] -> a
third1 = head . tail . tail

--b
third2:: [a] -> a
third2 xs = xs !! 2

--c
third3:: [a] -> a
third3 (x1:x2:x3:xs) = x3

--Exercise 3
--a
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then xs else tail xs

--b
safetail2 :: [a] -> [a]
safetail2 xs | null xs   = xs
             | otherwise = tail xs

--c
safetail3 :: [a] -> [a]
safetail3 []     = []
safetail3 (x:xs) = xs

--Exercise 4
(|||) :: Bool -> Bool -> Bool
--way 1
True |||  True = True
True ||| False = True
False||| True  = True
False||| False = False

{-
--way 2
False ||| False = False
_     ||| _     = True

--way 3
False ||| a = a
True  ||| _ = True

--way 4
a ||| a = a
_ ||| | = True

-}


--Exercise 5
(&&&) :: Bool -> Bool -> Bool
a &&& b = if a then 
                if b then True
                	else False 
          else False


--Exercise 7
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))

--Exercise 8
luhnDouble :: Int -> Int
luhnDouble n | n*2 < 9   = n*2
             | otherwise = n*2-9

luhn :: Int -> Int -> Int  -> Int -> Bool
luhn x1 x2 x3 x4 = (x2 + (luhnDouble x1) + x4 + (luhnDouble x3)) `mod` 10 == 0

{-Chapter 5-}
--Exercise 1 (a bit more advanced version than in the exercise, you can specify any number)
sum100 :: Int -> Int
sum100 xs = sum [x^2|x <- [1..xs]]

--Exercise 2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y)| x<-[0..m], y<- [0..n]]

--Exercise 3
square :: Int -> [(Int, Int)]
square n= [(x,y)| (x,y)<-grid n n, not (x==y)]

--Exercise 4
replic :: Int -> a -> [a]
replic n x = [x | _ <- [1..n]]

--Exercise 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y<-[1..n], z<-[1..n], x^2+y^2==z^2]

--Exercise 6
factors :: Int -> [Int]
factors n = [x |x<-[1..n], n `mod` x == 0 ]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (((sum (factors x))-x) == x)]

--Exercise 7
goodOne :: [(Int, Int)]
goodOne =concat [ [(x,y) | y <- [3,4] ] | x<-[1,2] ]

--Exercise 8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t , k==k']

positions :: Eq a => a -> [a] -> [Int]
positions n xs = find n (zip xs [0..])

--Exercise 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]