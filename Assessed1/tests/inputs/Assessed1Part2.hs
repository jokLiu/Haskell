{-# LANGUAGE Safe #-} 
module Assessed1Part2 where

-- We begin with a sample solution of part 1, produced by Cory.

import Data.Maybe
import Data.Char
import Text.Read 
import Data.List
import Data.Ord


data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

readBit :: Char -> Bit
readBit '0' = Z
readBit '1' = I

readBits :: [Char] -> [Bit]
readBits = map readBit

decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (tree, bits) = decodeAux tree tree bits

decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux fullTree (Leaf c _) [] = [c]
decodeAux fullTree (Leaf c _) bs = c:(decodeAux fullTree fullTree bs) 
decodeAux fullTree (Branch left right _) (Z:bs) = decodeAux fullTree left bs
decodeAux fullTree (Branch left right _) (I:bs) = decodeAux fullTree right bs

{- The input String has the following format:

   * An integer n coded as a sequence of digits.

   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}

decompress :: String -> String
decompress str = decode (t,bits)
    where
        (n',str') = span isDigit str
        n         = read n'
        t'        = take n str'
        t         = read t'
        bits      = readBits $ drop n str'

{- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding. -}

charlength :: Int
charlength = 8

-- gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' ('*':s)   = s
decompress' s = decompress s

-- Generate the frequency table
-- An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i

-- Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate = foldr update []

-- Removes the existing entry for c (if it exists), updates it, and
-- then reinserts it if no entry exists, we start over at 0, and then
-- "update"
update :: Eq c => c -> [Freq c] -> [Freq c]
update c keys = newFreq : rest
    where
        (old,rest) = (is c) `outOf` keys
        key = fromMaybe (c,0) old
        newFreq = mapSnd (+1) key

is :: Eq c => c -> Freq c -> Bool
is c (d,_) = c == d

outOf :: (a -> Bool) -> [a] -> (Maybe a,[a])
outOf p []     = (Nothing,[])
outOf p (x:xs) = if (p x) then (Just x,xs) else (mapSnd (x:) $ outOf p xs)

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (c,a) = (c, f a)

{- End of part 1. Your tasks for part 2 begin here. -}

-- Produce a Huffman tree from a list of Huffman trees.
-- https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
-- Question:
makeTree :: [Tree c] -> Tree c
makeTree = makeTreeHelper . sorting


sorting :: [Tree c] -> [Tree c]
sorting xs = sortBy (comparing takeValue) xs

-- Collects a list of trees into an optimal prefix tree.
makeTreeHelper :: [Tree c] -> Tree c
makeTreeHelper [x]        = x
makeTreeHelper (x1:x2:xs) = case (x1,x2) of 
                           (Leaf     _ a , Leaf     _ c) -> makeTreeHelper (put (Branch x1 x2 (a+c)) xs)
                           (Leaf     _ a , Branch _ _ c) -> makeTreeHelper (put (Branch x1 x2 (a+c)) xs)
                           (Branch _ _ a , Leaf     _ c) -> makeTreeHelper (put (Branch x1 x2 (a+c)) xs)
                           (Branch _ _ a , Branch _ _ c) -> makeTreeHelper (put (Branch x1 x2 (a+c)) xs)

--putting the new tree into the sorted list of trees into appropirate place
put :: Tree c -> [Tree c] -> [Tree c]
put x ls = [y | y <- ls, (takeValue x > takeValue y)] ++ [x] ++ [ y | y <- ls , (takeValue x <= takeValue y)]

--Taking the frequency value from a tree
takeValue :: Tree c -> Int
takeValue (Leaf     _ n)  = n
takeValue (Branch _ _ n)  = n



-- You may wish to use a helper function such as this:
merge :: Tree c -> Tree c -> Tree c
merge = undefined

-- Question:
-- Generate a tree from list of Freqs (using makeTree above):
generateTree :: [Freq c] -> Tree c
generateTree xs = makeTree [(Leaf c n) | (c,n) <- xs]




-- Encoding table.
-- A key is a key-value pair (an entry in a map/table).
type Key c = (c,[Bit])

-- The whole coding table
type CodingTable c = [Key c]

-- Question:
-- Given a tree, generates a coding table
makeTable :: Eq c => Tree c -> CodingTable c
makeTable tr = makeTableHelp tr []

makeTableHelp :: Eq c => Tree c -> [Bit]-> CodingTable c
makeTableHelp (Leaf a n)       xs   = [(a,xs)]
makeTableHelp (Branch t1 t2 a) xs   = (makeTableHelp t1 (xs++[Z])) ++ (makeTableHelp t2 (xs++[I]))







-- Question:
-- Takes a string of symbols to a bit string, based on a given coding table
encodeUsingTable :: Eq c => CodingTable c -> [c] -> [Bit]
encodeUsingTable tbl xs = concat [ ls | x <- xs, (a, ls)<-tbl, x==a]



-- Question:
-- Encodes directly from the tree (more efficient).
encodeUsing :: Eq c => Tree c -> [c] -> [Bit]
encodeUsing tr xs = concat (map (encodeHelp [(tr,[])]) xs )

--helper function for encoding from tree
encodeHelp :: Eq c => [((Tree c),[Bit])] -> c -> [Bit]
encodeHelp (((Leaf a b),ls):xss)       n    | n == a    = ls
                                            | otherwise = encodeHelp xss n
encodeHelp (((Branch t1 t2 b),ls):xss) n                = encodeHelp (xss++[(t1,(ls++[Z]))]++[(t2,(ls++[I]))]) n
encodeHelp []                          n                = []


-- Question:
-- From a string of symbols, generate the coding tree and the encoding
encode :: Eq c => [c] -> (Tree c, [Bit])
encode xs = (tr, bits)
              where bits  = encodeUsing tr xs
                    tr    = generateTree table 
                    table = tabulate xs


-- Encoding trees

-- Question:
-- Compressing a string. This should be the inverse of decompress.
-- That is, this should output a string of the form
--
-- n ++ t ++ c
--
-- Where,
--    * n is a read from an integer
--    * t is read from a tree, and contains exactly n characters.
--    * c is string of bits.
compress :: String -> String
compress xs = n ++ tr ++ bits
            where  n        = show len
                   len      = length tr
                   bits     = map readChar bit
                   tr       = show tre
                   (tre,bit)= encode xs 



readChar :: Bit -> Char
readChar  Z = '0'
readChar  I = '1'

-- Question:
-- Smarter compression: if the encoded string is larger than the input string,
-- instead output the input string with a '*' in front.
compress' :: String -> String
compress' xs = if (memSize xs < (memSize n + memSize tr+length bits)) then ("*"++xs)  else (n ++ tr ++ bits)
                where  n        = show len
                       len      = length tr
                       bits     = map readChar bit
                       tr       = show tre
                       (tre,bit)= encode xs 
