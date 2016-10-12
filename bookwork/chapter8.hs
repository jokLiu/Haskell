--Ex1
data Nat = Zero | Succ Nat

add:: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n) 

mult:: Nat -> Nat -> Nat
mult Zero     n = n
mult (Succ m) n = add n (mult m n)

--Ex2
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf v)      = x==v
occurs x (Node l v r)  = case compare x v of
	                        LT -> occurs x l
	                        EQ -> True
	                        GT -> occurs x r


--Ex3
data Treee a = Leaff a | Nodee (Treee a) (Treee a)

numberL :: Treee a -> Int 
numberL (Leaff _)   = 1
numberL (Nodee l r) = (numberL l) + (numberL r)

balanced :: Treee a -> Bool
balanced (Leaff _)   = True
balanced (Nodee l r)| abs((numberL l)-(numberL r)) >1 = False
                    | otherwise                       = (balanced l) && (balanced r)


--Ex4
halve :: [a] -> ([a],[a])
halve xs = (take first xs, drop first xs)
               where first = len `div` 2
            	  	where len   = length xs

balance :: [a] -> Treee a
balance []     = error "empty list"
balance [x]    = Leaff x
balance (xs)   = Nodee (balance y) (balance z)
                    where (y,z) = halve xs

--Ex5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a->a->a) -> Expr -> a
folde f g (Val x)         = f x 
folde f g (Add x1 x2)     =  g (folde f g x1) (folde f g x2)

--Ex6
eval :: Expr -> Int
eval (Val x)    = x
eval (Add x y)  = eval x + eval y

size :: Expr -> Int
size (Val _)    = 1
size (Add x y)  = size x + size y

--Ex7
{-
instance Eq a => Eq (Maybe a) where
Nothing === Nothing = True
Just a  === Just b  = a==b
_       === _       = False

-}
