import Data.List

{-Task 1-}
f :: Ord a => [a] -> [a]
f []     = []
f (x:xs) = f ys ++ [x] ++ f zs
              where ys = [a | a <- xs, a <= x]
                    zs = [a | a <- xs, a >  x]

{-Task 2-}
--All the tests including "f [3,2,1,0]" worked well

{-Task 3-}
--Tried to evaluate f [1..100000] and the time was exhausted
--while evaluating f [1..10000] took about 20seconds
--Evaluating f (reverse [1..10000] it took on avarage )

{-Task 4-}
--Evaluation of "[1..1000] ++ reverse [1..1000]" takes on avarage 2sec, and 2MB of memory


{-Task 5-}
--By testing the times experimentally, I figured out that the 
--time is few times larger than it should be
--I used the Haskell library permutations function for this task


--For all exercises I used my own machine
