{-# LANGUAGE Safe #-} -- For automatic marking to work.
module Assessed1Part1 where
import Text.Read 

{- Huffman Codes -}

data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

{--- Decoding ---}
-- Notice that this should work for types more general than Char (our c).
-- Question:
decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (t,ls) = decodeAux t t ls

-- You may or may not wish to use a helper function as follows for
-- decode (this function will not be marked, and you can leave it
-- undefined if you don't use it):
decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux (Leaf a b)            t2 xs                 = a : decodeAux t2 t2 xs
decodeAux t1                    t2 []                 = []
decodeAux (Branch left right a) t2 (x:xs) | x == Z    = decodeAux left t2 xs
                                          | otherwise = decodeAux right t2 xs
 

{-- decompression --}

{- The input String has the following format:

   * An integer n coded as a sequence of digits.
   
   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}

checkInt :: Char -> Maybe Int
checkInt a = (readMaybe [a] :: Maybe Int)

returnNumber :: String -> String
returnNumber []           = []
returnNumber (x:xs) | checkInt x /= Nothing = x : returnNumber xs
                    | otherwise             = []

readInteger ::String -> (Tree Char, String)
readInteger xs = (tr, ys)
                  where ys   = drop n temp
                        tr   = (read (take n temp) :: Tree Char)
                        temp = drop len xs
                        len  = length ls
                        n    = read ls :: Int
                        ls   = returnNumber xs


--functions which transfers list of '0's and '1's to list of Bit 
toBits :: String -> [Bit]
toBits []                 = []
toBits (x:xs) | x == '0'  = Z : toBits xs
              | otherwise = I : toBits xs


decompress :: String -> String
decompress xs = decode (tr,bits)
               where bits     = toBits seq
                     (tr,seq) = readInteger xs

{--- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding.  ---}

charlength :: Int
charlength = 8

--gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' = undefined


{--- Generate the frequency table ---}
--An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i

--Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate xs = freqHelp xs []

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
