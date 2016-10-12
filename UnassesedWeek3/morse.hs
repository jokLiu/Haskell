import MorseLib


--Question 1

--funtion to produce a Morse code for as single word
codeWord :: String -> [MorseUnit]
codeWord []     = []
codeWord (x:xs) = (codeSymbol x) ++ shortGap ++ (codeWord xs)

--function to produce to Morse code from a list of String
codeText :: [String] -> [MorseUnit]
codeText []     = []
codeText [x]    = codeWord x
codeText (x:xs) = (codeWord x) ++ mediumGap ++ (codeText xs)

--function that splits the String into a list of single word Strings
split :: String -> String -> [String] ->  [String]
split []     ys  zs               = zs++[ys]
split (x:xs) ys  zs   | x==' '    = split xs [] (zs++[ys])
                      | otherwise = split xs (ys++[x]) zs

--main function to encode String to Morse code
encode :: String -> [MorseUnit]
encode xs = codeText (split xs [] [])




--Question 2

--function which given a morse code splits it into a list of morse codes
--where each internal list represents a single character
decodeWords :: [MorseUnit] -> [MorseUnit] -> [[MorseUnit]] -> [[MorseUnit]] 
decodeWords []                          zs ls = ls
decodeWords (Beep:Silence:xs)           zs ls = decodeWords xs (zs++dit) ls
decodeWords (Beep:Beep:Beep:Silence:xs) zs ls = decodeWords xs (zs++dah) ls
decodeWords (Silence:Silence:xs)        zs ls = decodeWords xs [] (ls++[zs])

--function which given a a list of morse codes where each internal list
--represents a single letter produces a single word
decWordsToString :: [[MorseUnit]] -> String
decWordsToString xss = [z | xs <- xss, (zs,z) <- table, xs==zs]


--function which decodes a morse code into a single String
dec :: [MorseUnit] -> [MorseUnit] -> String ->String
dec []                                                   zs ls   = if zs == [] then ls else drop 1 (ls++" "++(decWordsToString (decodeWords zs [] [])))
dec (Beep:Silence:xs)                                    zs ls   = dec xs (zs++[Beep,Silence]) ls
dec (Beep:Beep:Beep:Silence:xs)                          zs ls   = dec xs (zs++[Beep,Beep,Beep,Silence]) ls
dec (Silence:Silence:Silence:Silence:Silence:Silence:xs) zs ls   = dec xs [] (ls++" "++(decWordsToString (decodeWords (zs++[Silence,Silence]) [] [])))
dec (Silence:Silence:xs)                                 zs ls   = dec xs (zs++[Silence,Silence]) ls


--main function for decoding Morse code to String
decode :: [MorseUnit] -> String 
decode xs = dec xs [] []






--Question 3

--FUnction for removing either dit or dah when one step is made
--For expample if it makes the dit move the function will remove all the first dit elements from the MorseTabel
--e.g [(dit,'a'),(dat,'b')] will become [([],'a'),(dat,'b')] if dit is passed as an argument
remove ::[MorseUnit] ->  MorseTable -> MorseTable
remove n []            = []
remove n ((xs,ch):xss) = if n == (take 2 xs) then ((drop 2 xs),ch) : remove n xss
                          else ((drop 4 xs),ch) : remove n xss

--Funtion for removing the character from the morse table when it was already put to the tree
--e.g we have [([],'a'),(dat,'b')], and we remove 'a' we will get [(dat,'b')]    
removeChar :: Char -> MorseTable -> MorseTable
removeChar n [] = []
removeChar n ((xs,ch):xss) | n==ch     = xss
                           | otherwise = (xs,ch) : removeChar n xss

--Funtion which checks if we reached an element(which doesn't need any more steps)
--e.g [([],'a'),(dat,'b')] will return True
inIt :: MorseTable -> Bool
inIt []                        = False
inIt ((xs,ch):xss) | xs == []  = True
                   | otherwise = inIt xss

--Funtion which returns the char which doesn' have any more steps to make
--e.g [([],'a'),(dat,'b')] will return 'a'
returnChar :: MorseTable -> Char
returnChar [] = 'A'--need to fix
returnChar ((xs,ch):xss) | xs == []  = ch
                         | otherwise = returnChar xss


--Function which seperates MorseTable into two tables where the first one
--represents table with all elements which starts with dit symbol, and another with dah
seperate :: MorseTable -> (MorseTable,MorseTable) -> (MorseTable,MorseTable)
seperate []     (xss,yss)         = (xss,yss)
seperate ((ls,ch):lss) (xss,yss) | (take 2 ls) == dit = seperate lss ((xss++[(ls,ch)]),yss)
                                 | otherwise          = seperate lss (xss,(yss++[(ls,ch)]))

--Function which bilds the MorseTree from MorseTabel leaving the last elements as Nil
toTreeHelper :: MorseTable -> MorseTree
toTreeHelper []              = Nil
toTreeHelper xs |   inIt xs  = Branch1 (returnChar xs) (toTreeHelper (remove dit left{-(removeChar (returnChar xs) xs )-})) (toTreeHelper (remove dah right{-(removeChar (returnChar xs) xs )-})) 
                | otherwise  = Branch0 (toTreeHelper (remove dit left)) (toTreeHelper (remove dah right))
                                 where (left,right) = if not (inIt xs) then seperate xs ([],[])
                                                       else  (seperate (removeChar (returnChar xs) xs ) ([],[]))

--Function which transfers all Branch x Nil Nil to Leaf x
removeNiltoLeaf :: MorseTree -> MorseTree
removeNiltoLeaf Nil               = Nil
removeNiltoLeaf (Leaf x)          = Leaf x 
removeNiltoLeaf (Branch1 x t1 t2) = if (t1 == Nil && t2 == Nil) then Leaf x else Branch1 x (removeNiltoLeaf t1) (removeNiltoLeaf t2)
removeNiltoLeaf (Branch0 t1 t2)   = Branch0 (removeNiltoLeaf t1) (removeNiltoLeaf t2) 

--Main toTree function
toTree :: MorseTable -> MorseTree
toTree = removeNiltoLeaf . toTreeHelper




--Question 4

--function which produces the MorseTable with duplicate values
toTableHelper :: MorseTree -> [MorseUnit] -> MorseTable -> MorseTable
toTableHelper Nil               ls tbl = tbl
toTableHelper (Leaf x)          ls tbl = tbl++[(ls,x)]
toTableHelper (Branch1 x t1 t2) ls tbl = (toTableHelper t1 (ls++dit) (tbl++[(ls,x)]))++(toTableHelper t2 (ls++dah) tbl)
toTableHelper (Branch0 t1 t2)   ls tbl = (toTableHelper t1 (ls++dit) tbl)++(toTableHelper t2 (ls++dah) tbl)

--helper function for removing duplicates
rmdupHelp :: MorseTable ->Char -> MorseTable
rmdupHelp []            _             = []
rmdupHelp ((xs,ch):xss) n | ch == n   = rmdupHelp xss n
                          | otherwise = (xs,ch): (rmdupHelp xss n)

--function fo removing duplicates from the table
rmdup :: MorseTable -> MorseTable
rmdup []     = []
rmdup ((x,ch):xs) = (x,ch) : (rmdup (rmdupHelp xs ch))

--main to table function
toTable :: MorseTree -> MorseTable
toTable xs = rmdup (toTableHelper xs [] [])


--All tests

--inputs and uotputs for testing
-- Q1.1
input1 :: [String]
input1 = ["HELLO WORLD", "3CPO R2D2", "62450875", "THIS IS A LONG SENTENCE CONTAINING NUMBERS 0 12 135"]

output1 :: [[MorseUnit]]
output1 = [[Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence],[Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence],[Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence],[Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Silence,Silence,Silence,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Silence,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Silence,Silence]]

-- Q1.2
input2 :: [[MorseUnit]]
input2 = output1

output2 :: [String]
output2 = input1

-- Q1.3
input3 :: [MorseTable]
input3 = table : ((dit ++ dah ++ dit ++ dah ++ dit ++ dah, '.') : (dah ++ dah ++ dit ++ dit ++ dah ++ dah, ',') : table) : []

output3 :: [MorseTree]
output3 = [Branch0 (Branch1 'E' (Branch1 'I' (Branch1 'S' (Branch1 'H' (Leaf '5') (Leaf '4')) (Branch1 'V' Nil (Leaf '3'))) (Branch1 'U' (Leaf 'F') (Branch0 Nil (Leaf '2')))) (Branch1 'A' (Branch1 'R' (Leaf 'L') Nil) (Branch1 'W' (Leaf 'P') (Branch1 'J' Nil (Leaf '1'))))) (Branch1 'T' (Branch1 'N' (Branch1 'D' (Branch1 'B' (Leaf '6') Nil) (Leaf 'X')) (Branch1 'K' (Leaf 'C') (Leaf 'Y'))) (Branch1 'M' (Branch1 'G' (Branch1 'Z' (Leaf '7') Nil) (Leaf 'Q')) (Branch1 'O' (Branch0 (Leaf '8') Nil) (Branch0 (Leaf '9') (Leaf '0'))))),Branch0 (Branch1 'E' (Branch1 'I' (Branch1 'S' (Branch1 'H' (Leaf '5') (Leaf '4')) (Branch1 'V' Nil (Leaf '3'))) (Branch1 'U' (Leaf 'F') (Branch0 Nil (Leaf '2')))) (Branch1 'A' (Branch1 'R' (Leaf 'L') (Branch0 (Branch0 Nil (Leaf '.')) Nil)) (Branch1 'W' (Leaf 'P') (Branch1 'J' Nil (Leaf '1'))))) (Branch1 'T' (Branch1 'N' (Branch1 'D' (Branch1 'B' (Leaf '6') Nil) (Leaf 'X')) (Branch1 'K' (Leaf 'C') (Leaf 'Y'))) (Branch1 'M' (Branch1 'G' (Branch1 'Z' (Leaf '7') (Branch0 Nil (Leaf ','))) (Leaf 'Q')) (Branch1 'O' (Branch0 (Leaf '8') Nil) (Branch0 (Leaf '9') (Leaf '0')))))]

-- Q1.4
input4 :: [MorseTree]
input4 = output3

output4 :: [MorseTable]
output4 = [[([Beep,Silence],'E'),([Beep,Silence,Beep,Silence],'I'),([Beep,Silence,Beep,Silence,Beep,Silence],'S'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'H'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'5'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'4'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'V'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'3'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'U'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'F'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'2'),([Beep,Silence,Beep,Beep,Beep,Silence],'A'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'R'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'L'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'W'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'P'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'J'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'1'),([Beep,Beep,Beep,Silence],'T'),([Beep,Beep,Beep,Silence,Beep,Silence],'N'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'D'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'B'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'6'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'X'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'K'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'C'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'Y'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'M'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'G'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'Z'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'7'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'Q'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'O'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'8'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'9'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'0')],[([Beep,Silence],'E'),([Beep,Silence,Beep,Silence],'I'),([Beep,Silence,Beep,Silence,Beep,Silence],'S'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'H'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'5'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'4'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'V'),([Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'3'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'U'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'F'),([Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'2'),([Beep,Silence,Beep,Beep,Beep,Silence],'A'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'R'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'L'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'.'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'W'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'P'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'J'),([Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'1'),([Beep,Beep,Beep,Silence],'T'),([Beep,Beep,Beep,Silence,Beep,Silence],'N'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'D'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'B'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'6'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'X'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'K'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'C'),([Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'Y'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'M'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'G'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'Z'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Silence],'7'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],','),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Beep,Beep,Silence],'Q'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'O'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence,Beep,Silence],'8'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Silence],'9'),([Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence,Beep,Beep,Beep,Silence],'0')]]

--tests for each part
test1 :: Bool
test1 = (map encode input1) == output1

test2 :: Bool
test2 = (map decode input2) == output2

test3 :: Bool
test3 = (map toTree input3) == output3

test4 :: Bool
test4 = (map toTable input4) == output4

--testing all test at once
testAll :: Bool
testAll = test1 && test2 && test3 && test4
