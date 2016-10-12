--1
import Data.List
import Data.Maybe
s ::  [Int] -> [Int]
s = map (\x -> x^2) . filter (\x -> x `mod` 2 == 0)

--2
--b
anyy :: (a -> Bool) -> [a] -> Bool
anyy p = or . map p

--c
takeW :: (a-> Bool) -> [a] -> [a]
takeW p []                 = []
takeW p (x:xs) | p x       = x : takeW p xs
               | otherwise = takeW p xs  

--d
dropW :: (a-> Bool) -> [a] -> [a]
dropW p []                 = []
dropW p (x:xs) | p x       = dropW p xs
               | otherwise = x: dropW p xs

--3
filt :: (a->Bool) -> [a] -> [a]
filt p = foldr (\x xs -> if p x then x:xs else xs) []

--4
dec2Int :: [Int] -> Int 
dec2Int = foldl (\x y -> x*10+y) 0



unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)


--6
type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (==[]) (take 8) (drop 8)

mappp :: (a->b)->[a] -> [b]
mappp f = unfold null (f.head) tail

altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap f g xs | null xs     = []
              | otherwise   = (f (head xs)) : altMap g f (tail xs)

luhn :: [Int] -> Bool
luhn xs= (sum (map (\x -> if x>9 then (x-9) else x) (altMap (+0) (*2) (reverse xs)))) `mod` 10 == 0
