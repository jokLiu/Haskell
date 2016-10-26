module Huffman where


--Function which counts the number of occurances of particular element
--and removes them from the String
countRemove :: String -> Char -> Int -> String -> (Int, String)
countRemove []     ch n ls             = (n,ls)
countRemove (x:xs) ch n ls | x==ch     = countRemove xs ch (n+1) ls
                           | otherwise = countRemove xs ch n     (ls++[x]) 

--Function which counts all occurances in the String
freqHelp :: String -> [(Char,Int)] -> [(Char,Int)]
freqHelp []     ls = ls
freqHelp (x:xs) ls = freqHelp zs ((x,num):ls)
                   where (num,zs) = countRemove xs x 1 []

--main Occurance function
freq :: String -> [(Char, Int)]
freq xs = freqHelp xs [] 

-------------
--sorting part

--Splitting the list for mergesort algorithm
split :: [a] -> ([a],[a])
split []         = ([],[])
split (x:[])     = ([x],[])
split (x1:x2:xs) = (x1:xs1 , x2:xs2)
                         where (xs1,xs2) = split xs

--merging the two list for mergsort algorithm
merge :: ([(Char,Int)],[(Char,Int)]) -> [(Char,Int)]
merge (xs,[])                          = xs
merge ([],ys)                          = ys
merge ((a,x):xs , (b,y):ys) | x>y      = (a,x) : merge (xs, (b,y):ys)
                            |otherwise = (b,y) : merge ((a,x):xs, ys)

--mergesort algorithm which works O(nlogn) complexity
mergesort :: [(Char,Int)] -> [(Char,Int)]
mergesort []   = []
mergesort [x]  = [x]
mergesort xs   = merge (zs, ys) 
                 where zs         = mergesort xs1
                       ys         = mergesort xs2
                       (xs1,xs2) = split xs 
                      

--Huffmane Tree

--definition of the tree
data HuffTree = Empty
	            | End Int Char
                | Branch Int (HuffTree) (HuffTree) deriving Show


--function for taking the value of the current tree (number of frequencies)
takeValue :: HuffTree -> Int
takeValue Empty            = 0
takeValue (End a b)        = a
takeValue (Branch a t1 t2) = a


--from list of char and int to list of Huffman Trees
toHuffList :: [(Char,Int)] -> [HuffTree]
toHuffList []         = []
toHuffList ((a,b):xs) = (End b a) : toHuffList xs



--function for putting element into the right place in the list
put :: HuffTree -> [HuffTree] -> [HuffTree]
put x ls = [y |  y <- ls , (takeValue x > takeValue y)] ++ [x] ++ [y |  y <- ls , (takeValue x <= takeValue y)]


--function which builds a tree 
buildTree :: [HuffTree] -> HuffTree
buildTree []         = Empty
buildTree [x]        = x
buildTree (x1:x2:xs) = case (x1,x2) of
                               (End a _, End c _)               -> buildTree (put (Branch (a+c) x1 x2) xs)
                               (End a _, Branch c _ _)          -> buildTree (put (Branch (a+c) x1 x2) xs)
                               (Branch a _ _, End c _)          -> buildTree (put (Branch (a+c) x1 x2) xs)
                               (Branch a _ _, Branch c _ _)     -> buildTree (put (Branch (a+c) x1 x2) xs)


--building a table of elements and their code sequences
buildTable :: HuffTree -> (String,Char) -> [(String,Char)]
buildTable (End a b)        (str,_)    =  [(str,b)]
buildTable (Branch a t1 t2) (str,x)    = (buildTable t1 ((str++"0"),x))++(buildTable t2 ((str++"1"),x))


buildTableMain :: HuffTree -> [(String, Char)]
buildTableMain tr = buildTable tr ("",' ')


--function from String to bit
stringToBit ::String -> [(String,Char)]  -> String
stringToBit xs ls = concat [ str | x <- xs, (str,ch) <- ls, x==ch]


--function from bit to String
bitToString :: String -> HuffTree -> String
bitToString xs t = bitToStringHelp xs t t


bitToStringHelp :: String -> HuffTree -> HuffTree -> String
bitToStringHelp xs     (End a b) t2                    = b : bitToStringHelp xs t2 t2
bitToStringHelp []     t1 t2                           = ""
bitToStringHelp (x:xs) (Branch a t3 t4) t2 | x == '0'  = bitToStringHelp xs t3 t2
                                           | otherwise = bitToStringHelp xs t4 t2







------------------------------
--code from the assignment 
--sorting

-Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate xs = sorting ( freqHelp xs [])

--helper function for tabulate
freqHelp :: Eq c => [c] -> [Freq c] -> [Freq c]
freqHelp []     ls = ls
freqHelp (x:xs) ls = freqHelp zs ((x,num):ls)
                     where (num,zs) = countRemove xs x 1 []

--Function which counts the number of occurances of particular element
--and removes them from the list of elements [c]
countRemove :: Eq c => [c]  -> c -> Int -> [c] -> (Int, [c])
countRemove []     ch n ls             =  (n,ls)                         
countRemove (x:xs) ch n ls | x==ch     =  countRemove xs ch (n+1) ls
                           | otherwise =  countRemove xs ch n     (ls++[x])

sorting :: [Freq c] -> [Freq c]
sorting =  reverse . sortBy (comparing snd) 