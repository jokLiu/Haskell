--1
fac 0             = 1
fac n | n<0       = 0
      | otherwise = n*(fac (n-1))

--2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + (sumdown (n-1))

--3
(^^^) :: Int -> Int -> Int
n ^^^ 0 = 1
n ^^^ m = n*(n^^^(m-1))

--4
euclid :: Int -> Int -> Int
euclid 1 m                       = 1
euclid n 1                       = 1
euclid n m | ( n == 0 || m == 0) = 0
           | n==m                = n
           | n>m                 = euclid (n-m) m
           | n<m                 = euclid n (m-n)

--6
--a
an :: [Bool] -> Bool
an []                 = True
an (x:xs) | x==True   = an xs
          | otherwise = False

--b
conca :: [[a]] -> [a]
conca []     = []
conca (x:xs) = x++(conca xs)

--c
repl :: Int -> a -> [a]
repl 0 _ = []
repl n m = [m] ++ (repl (n-1) m)

--d
(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = (xs !!! (n-1))

--e
el :: Eq a => a -> [a] -> Bool 
el n []     = False
el n (x:xs) | n==x      = True
			| otherwise = el n xs

--7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys                     = ys
merge xs []                     = xs
merge (x:xs) (y:ys) | x>y       = y: merge (x:xs) ys
					| otherwise = x: merge xs (y:ys)
