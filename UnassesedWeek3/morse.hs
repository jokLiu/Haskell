import MorseLib


codeWord :: String -> [MorseUnit]
codeWord []     = []
codeWord (x:xs) = (codeSymbol x) ++ shortGap ++ (codeWord xs)

codeText :: [String] -> [MorseUnit]
codeText []     = []
codeText [x]    = codeWord x
codeText (x:xs) = (codeWord x) ++ mediumGap ++ (codeText xs)


split :: String -> String -> [String] ->  [String]
split []     ys  zs               = zs++[ys]
split (x:xs) ys  zs   | x==' '    = split xs [] (zs++[ys])
                      | otherwise = split xs (ys++[x]) zs

encode :: String -> [MorseUnit]
encode xs = codeText (split xs [] [])

input1 :: [String] 
input1 = ["HELLO WORLD", "3CPO R2D2", "62450875", "THIS IS A LONG SENTENCE CONTAINING NUMBERS 0 12 135"]

output1 :: [[MorseUnit]]
output1 = [[Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence],[Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence],[Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence],[Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence]]



decodeWords :: [MorseUnit] -> [MorseUnit] -> [[MorseUnit]] -> [[MorseUnit]] 
decodeWords []                 zs ls          = ls
decodeWords (Beep:Silence:xs)  zs ls          = decodeWords xs (zs++dit) ls
decodeWords (Beep:Beep:Beep:Silence:xs) zs ls = decodeWords xs (zs++dah) ls
decodeWords (Silence:Silence:xs)        zs ls = decodeWords xs [] (ls++[zs])

decWordsToString :: [[MorseUnit]] -> String
decWordsToString xss = [z | xs <- xss, (zs,z) <- table, xs==zs]


s :: MorseUnit
s = Silence

b :: MorseUnit
b = Beep 


dec :: [MorseUnit] -> [MorseUnit] -> String ->String
dec [] zs ls                 = if zs == [] then ls else drop 1 (ls++" "++(decWordsToString (decodeWords zs [] [])))
dec (Beep:Silence:xs)  zs ls          = dec xs (zs++[Beep,Silence]) ls
dec (Beep:Beep:Beep:Silence:xs) zs ls       = dec xs (zs++[Beep,Beep,Beep,Silence]) ls
dec (Silence:Silence:Silence:Silence:Silence:Silence:xs) zs ls       = dec xs [] (ls++" "++(decWordsToString (decodeWords (zs++[Silence,Silence]) [] [])))
dec (Silence:Silence:xs)        zs ls    = dec xs (zs++[Silence,Silence]) ls

a :: [MorseUnit]
a = encode "LABAS MANO VARDAS KRABASS"

e:: [[MorseUnit]]
e = decodeWords a [] []

c :: String
c = dec a [] []



--Ex 3



remove ::[MorseUnit] ->  MorseTable -> MorseTable
remove n []            = []
remove n ((xs,ch):xss) = if n == (take 2 xs) then ((drop 2 xs),ch) : remove n xss
	                     else ((drop 4 xs),ch) : remove n xss
	                     
removeChar :: Char -> MorseTable -> MorseTable
removeChar n [] = []
removeChar n ((xs,ch):xss) | n==ch     = xss
                           | otherwise = (xs,ch) : removeChar n xss

inIt :: MorseTable -> Bool
inIt []                        = False
inIt ((xs,ch):xss) | xs == []  = True
                   | otherwise = inIt xss

returnChar :: MorseTable -> Char
returnChar [] = 'A'--need to fix
returnChar ((xs,ch):xss) | xs == []  = ch
                         | otherwise = returnChar xss

seperate :: MorseTable -> (MorseTable,MorseTable) -> (MorseTable,MorseTable)
seperate []     (xss,yss)         = (xss,yss)
seperate ((ls,ch):lss) (xss,yss) | (take 2 ls) == dit = seperate lss ((xss++[(ls,ch)]),yss)
                                 | otherwise          = seperate lss (xss,(yss++[(ls,ch)]))



toTree :: MorseTable -> MorseTree
toTree []              = Nil
toTree xs |   inIt xs  = Branch1 (returnChar xs) (toTree (remove dit left{-(removeChar (returnChar xs) xs )-})) (toTree (remove dah right{-(removeChar (returnChar xs) xs )-})) 
	      | otherwise  = Branch0 (toTree (remove dit left)) (toTree (remove dah right))
                                 where (left,right) = if not (inIt xs) then seperate xs ([],[])
                                                       else  (seperate (removeChar (returnChar xs) xs ) ([],[]))

toTreeHelp :: MorseTree -> MorseTree
toTreeHelp Nil               = Nil
toTreeHelp (Leaf x)          = Leaf x 
toTreeHelp (Branch1 x t1 t2) = if (t1 == Nil && t2 == Nil) then Leaf x else Branch1 x (toTreeHelp t1) (toTreeHelp t2)
toTreeHelp (Branch0 t1 t2)   = Branch0 (toTreeHelp t1) (toTreeHelp t2) 

toTreeFinal :: MorseTable -> MorseTree
toTreeFinal = toTreeHelp . toTree

-- Q1.3
input3 :: [MorseTable]
input3 = table : ((dit ++ dah ++ dit ++ dah ++ dit ++ dah, '.') : (dah ++ dah ++ dit ++ dit ++ dah ++ dah, ',') : table) : []

output3 :: [MorseTree]
output3 = [Branch0 (Branch1 'E' (Branch1 'I' (Branch1 'S' (Branch1 'H' (Leaf '5') (Leaf '4')) (Branch1 'V' Nil (Leaf '3'))) (Branch1 'U' (Leaf 'F') (Branch0 Nil (Leaf '2')))) (Branch1 'A' (Branch1 'R' (Leaf 'L') Nil) (Branch1 'W' (Leaf 'P') (Branch1 'J' Nil (Leaf '1'))))) (Branch1 'T' (Branch1 'N' (Branch1 'D' (Branch1 'B' (Leaf '6') Nil) (Leaf 'X')) (Branch1 'K' (Leaf 'C') (Leaf 'Y'))) (Branch1 'M' (Branch1 'G' (Branch1 'Z' (Leaf '7') Nil) (Leaf 'Q')) (Branch1 'O' (Branch0 (Leaf '8') Nil) (Branch0 (Leaf '9') (Leaf '0'))))),Branch0 (Branch1 'E' (Branch1 'I' (Branch1 'S' (Branch1 'H' (Leaf '5') (Leaf '4')) (Branch1 'V' Nil (Leaf '3'))) (Branch1 'U' (Leaf 'F') (Branch0 Nil (Leaf '2')))) (Branch1 'A' (Branch1 'R' (Leaf 'L') (Branch0 (Branch0 Nil (Leaf '.')) Nil)) (Branch1 'W' (Leaf 'P') (Branch1 'J' Nil (Leaf '1'))))) (Branch1 'T' (Branch1 'N' (Branch1 'D' (Branch1 'B' (Leaf '6') Nil) (Leaf 'X')) (Branch1 'K' (Leaf 'C') (Leaf 'Y'))) (Branch1 'M' (Branch1 'G' (Branch1 'Z' (Leaf '7') (Branch0 Nil (Leaf ','))) (Leaf 'Q')) (Branch1 'O' (Branch0 (Leaf '8') Nil) (Branch0 (Leaf '9') (Leaf '0')))))]


test3 :: Bool
test3 = (map toTreeFinal input3)== output3


test4 =  toTreeFinal [(dit,'E'),(dah, 'T'),(dit ++ dit, 'I'),(dit ++ dah, 'A'),(dah ++ dah, 'M'), (dah ++ dit, 'N')]
--remove dit [([Beep,Silence],'E'),([Beep,Silence,Beep,Silence],'I'),([Beep,Silence,Beep,Beep,Beep,Silence],'A')]
-- seperate [(dit,'E'),(dah, 'T'),(dit ++ dit, 'I'),(dit ++ dah, 'A'),(dah ++ dah, 'M'), (dah ++ dit, 'N')] ([],[])

--([([Beep,Silence],'E'),([Beep,Silence,Beep,Silence],'I'),([Beep,Silence,Beep,Beep,Beep,Silence],'A')],[([Beep,Beep,Beep,Silence],'T'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'M'),([Beep,Beep,Beep,Silence,Beep,Silence],'N')])