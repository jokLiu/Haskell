{-# LANGUAGE Safe #-} -- For automatic marking to work.
module Assessed1Part1 where
import Text.Read 
import Data.List
import Data.Ord
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
decompress' []    = []
decompress' (x:xs) | x=='*'     = xs
                   | otherwise  = decompress (x:xs)


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

compressTest :: Bool
compressTest = uncompressedString == decompress' compressedString

-- We will also test other examples for markink.

uncompressedString = "US officials are investigating multiple attacks that caused widespread online disruption on both sides of the Atlantic on Friday.\n\nThe Department of Homeland Security has begun an investigation into the DDoS (distributed denial-of-service) attack, the Guardian confirmed.\n\nThe incident took offline some of the most popular sites on the web, including Netflix, Twitter, Spotify, Reddit, CNN, PayPal, Pinterest and Fox News - as well as newspapers including the Guardian, the New York Times and the Wall Street Journal.\n\nThe attacks seemed to have been focused on Dyn, the company that runs the internet's domain name system (DNS).\n\nAmazon's web services division, one of the world's biggest cloud computing companies, also reported an outage that lasted several hours on Friday morning.\nSign up to the new-look Media Briefing: bigger, better, brighter\nRead more\n\nDoug Madory, director of internet analysis at Dyn, said he was not sure if the outages at Dyn and Amazon were connected.\n\n'We provide service to Amazon, but theirs is a complex network so it is hard to be definitive about causality,' he said.\n\nAmazon was not available for comment.\n\nDyn said it first became aware of the attack shortly after 7am ET on Friday. 'We began monitoring and mitigating a DDoS [distributed denial-of-service] attack against our Dyn Managed DNS infrastructure,' the company said on its website.\n\nThe company sent out updates throughout the day, confirming a second attack at about noon and a third just after 4pm.\n\nDDoS attacks are also becoming more common. Brian Krebs, an independentsecurity researcher, observed earlier this month that the 'source code' to the Mirai botnet had been released by a hacker group, 'virtually guaranteeing that the internet will soon be flooded with attacks from many new botnets powered by insecure routers, IP cameras, digital video recorders and other easily hackable devices'"

compressedString = 
  "1474Branch (Branch (Branch (Leaf 'e' 179) (Branch (Branch (Branch (Branch (Branch (Leaf 'A' 5) (Branch (Branch (Leaf 'I' 1) (Branch (Leaf '4' 1) (Leaf 'K' 1) 2) 3) (Leaf 'x' 3) 6) 11) (Branch (Leaf 'U' 1) (Leaf 'S' 10) 11) 22) (Leaf '\\n' 22) 44) (Branch (Branch (Leaf '\\'' 11) (Leaf '.' 12) 23) (Leaf 'p' 23) 46) 90) (Leaf 'r' 91) 181) 360) (Branch (Branch (Branch (Leaf 'c' 49) (Leaf 'h' 49) 98) (Branch (Branch (Leaf ',' 25) (Leaf 'y' 26) 51) (Branch (Branch (Branch (Leaf '-' 6) (Branch (Leaf 'W' 3) (Branch (Leaf 'R' 2) (Leaf 'B' 2) 4) 7) 13) (Branch (Leaf 'T' 7) (Leaf 'N' 7) 14) 27) (Leaf 'f' 27) 54) 105) 203) (Branch (Leaf 'n' 111) (Leaf 'o' 112) 223) 426) 786) (Branch (Branch (Branch (Leaf 'i' 113) (Branch (Branch (Branch (Leaf 'k' 14) (Leaf 'D' 15) 29) (Leaf 'b' 29) 58) (Branch (Leaf 'g' 31) (Branch (Branch (Branch (Branch (Branch (Leaf ':' 1) (Leaf '7' 1) 2) (Branch (Leaf 'Y' 1) (Leaf 'J' 1) 2) 4) (Branch (Branch (Leaf ']' 1) (Leaf 'j' 1) 2) (Branch (Leaf 'E' 1) (Leaf '[' 1) 2) 4) 8) (Branch (Branch (Leaf ')' 2) (Leaf 'G' 2) 4) (Branch (Branch (Leaf 'H' 1) (Leaf 'C' 1) 2) (Leaf '(' 2) 4) 8) 16) (Branch (Branch (Leaf 'z' 4) (Leaf 'M' 4) 8) (Branch (Leaf 'F' 4) (Leaf 'P' 4) 8) 16) 32) 63) 121) 234) (Branch (Leaf 'a' 134) (Branch (Leaf 'd' 67) (Branch (Branch (Leaf 'v' 16) (Leaf 'w' 20) 36) (Leaf 'm' 39) 75) 142) 276) 510) (Branch (Leaf ' ' 287) (Branch (Leaf 't' 146) (Branch (Branch (Leaf 'l' 42) (Leaf 'u' 42) 84) (Leaf 's' 87) 171) 317) 604) 1114) 190000100010001000111100111010111010111100001000100010101111001111111010100011000110100001101011100000111111110100010011010101110100001101001101101011111111011111001110100000101111110000011010101110111010100100010010001111111011100100110101110110010001010111101111110001011011010111011000101100001111100101100110001010101101100111011011110010000110000110101101000111110011111101001011111010000111011011001110110110100101011111100100111011111100010110000111111100111010111110111001001000110001000001110111100101001101110100001000110011101101101001111100011100010110101001010100101010010010010010101101001001000110100100100000101110100011111010111100001101110110011101011111010011101100011110111100011110010100110101101100010001100001000111101001110001110010101110010011010111111101001010001001101111010110110101001101101000011010111000001111111101000100110101011101000011101101101000011011100111110111001001000110100100110010010111001000111101001110111101101000111111110001110001001011111011110000101101101011000001101000101011110001011000011101011101011000111110000011101110010000100000010011101001101010111011101010010001001000010100110111001001000110100111010111110110100011101101000101001101100100001110110010111100000111011110001011000101010010010010010101101001001000110100001100100010001011000001101110110111001110111100100011001110101110101111111001000011000011011111011110111100011001110101111101110010010001101011110111111111110110001011011100101111110111110010100011110111111000111000011111110011101101101110010010001101011101000100101010100110100001100100011110011110110110100001101001101100101101100011100101111111001000001000011010100110010110101011101100011101110000001101010011000100011001011011111101000010111010101010100110010110011000010110101101000111001010011010011101101010110110101101101010011010011111110100101011001111111010111100010100110100111111100001101110000001100011111111011010100110101101101001111100111001000011110010110110001011101111111100101100011010101111111010111010001111001111001101010111111100110000101110111111001011101000101100000111111111010000110010001111001111011011010000110100110110111001001000110100111010111110110100011101101000101001100101001101110010010001100101101100010111011101001110001001110011100100011001011010100010111100011111110101001101011011011100100100011001011001010101111001111001100010001111100011000000111011010011100011011111110100110110101011110000101010010010010010101101001001000110101011101110101001000100100011111110111110000001011110001011011011100111110010011010101110000011010010100000001101100101110111010001111011111100010110110011101101101001001010101011001010011011100100100011001000011110111100101110100110010101110111001001101011101100011111101011011111110111001001000110100001101110000001101100001110001010011111110101100111101111101010000110110011010101011110001101111101010111111111000010111111010011101111001001010110110010001110011101000010101001001001001001000001011111010100111100011101100010100111111101011101000100101110111110000011101110010000100000011111110101101000101110010001111110000111011001010011001110110000110011101011111011100100100011010111010111001111110010110001010011111110100101100010011010011000011111111011001000111100011111110110110110010000111101111001011111101111010000110100110110010000111101111001011101001101000000111110101001101010111100111110111110001100000101101110011111000010110110101001101100111111101111010101001100001101110010011010111011011110010101111111100001011011011111000101110000000111010111100110010010111111101001111111110011101101101001111100011100010110101001010111010111101110011011010000110100110001010100100100100011100010011001101101111010010111101110011111011100100100011001100001011101010110001111000111011110010001101001111010001011010001010110010110011100111000000010111100001101001101001110000011010010110001001101001100000011010100110100101000111011100000011010100110100101001110001001100100111100000011001001010110011000010101011011010111101110011000001001001001100100101111111011001101101001111011010101100111001101010101010011010110100000110000100011100111001111001110101111101000011011100000011011000011101101010011010101111000101011111110001111111010101110110100100101010101100101001101111110101000101101100100100011010111011010111111100110011111101101111111110100110001101000010111110111001001000110011111110111101010100110000111111101010111011010010010101010110110101001101011011000100000101111101010011110001110110110101110100000110001100100001110110011000001000111000010110001010100100100100100101000101100100001100010110011011110111001000101100001101111100000111011100100001000000110111001111100010000010111110101001111000111011001010011010010111110111101101110010010001000001111111110100011111110101011001000011110111100101111110000000100001111001100001110101110101110011100100011011111011111010001110110100011111110010011010001110110110111001111101001010001101011000001011110000110100011101000101110000011010101001010111111101111011001000101011110111111101011110010001110010101010100001010011001001000110111111010100010110001010100100100100100100000101111101010011110001110110110101110110101111111001100111111011010101011100101010001111001010100101111100000110010111011100111100100001111011111011110000110111000101010010010010011001001010101011011011111101010001011011010001110110010111100000111111111101101001010000100010101011110001101010101110110100011000110011101011111011100100100011010101110111010100100010010001101111101001011100111110111100010101110101001011111100000011110100111000011010101111110100111001100101101011001110110110100111110001110001011010100101010010101110001010001011001000011010010100010011010100110110101111011101101000111001110011100001101001101101010011010110110101111100011101000100110101011101000011010011011010101101001001100100101110010001111010011100111101101000111111110001110001001011111011110000101101101011000001101000101011110001011000011101011101011000111110000011101110010000100000010011100100110101011101110101001000100100011010101001101010100001101111111101100111111101001111010010010101010110110100111101101001101010100110000101101101001001010110110010001111010000110010111001110101111111100011111101010001110111101001100001010000101001101110010010001100100001111011110010111010011001010111011111101010001011011001110110110100011101111111010111010001001011111110001110000001010100100100100101011010010010001100100001111011110010111010011001010111011111000011011101100111111101111011011110100101110110101011100001111111011100100100110111111101100110010010111111101111011011100100100011010110101001010101010011001000011101100101111000001110111110000110100110110101011011111000010000111011010110110101011101110101001000100100011010101110110101010010101111111011110110011001110111011011010100110101101101010110111001001100000111011011010011100101111101111111110110101001011111100000011110001000010100010111011110010101001001001001100100110010010111001000111101010111011101010010001001000111111101010001100011010101111001111101111101001010000100001111011111000011010011011010111101110011000110010000111101111101111011101100010101110010110011100111000101001101100010000101100110001001011111101010011010100110110100001101011000000101100001101011000001101110111110000100011110100111000111001010111000110001111100010100011010000100100000110101001100111100101111110000011101110000010110110000101000111111001000000001111011100100110001111111010111101110110111001001110111001001101011101101110010010001100010100111110111111101001101000000110010000111101100000010100110111001111101110010010001101001111011000001110101000110100101011111100110000111011001001101010110110100101000000011011000110001111000001010111110001011011010010101010111010101100100110100100010010000000011110100110001101111111010010110101001100010100101110010000011111011110110101111001111000101011101001101111011010001110100110111000000010000110100110110111001001101011101101110010010001101000011011100000011011000011101101011101100011110011110011011111011101110110110100101000110010111111100011101111011000010110110101110110001110010011101010111011101010010001001000111111100101110011011110111111010111110100110010101110011000010111011101001010111111001100001110111111100010110111101110100000110001011011010010101010111010000110111110000100011110100110001100011011111110111100000011111110101001100010000100100111111110010001010101111000001110101111101010011010110100010011010001110101011110011010111001000101100000111110001100001000011100111011000000111111111010100110101101100111111001001000001111000010101111110001111000101011100100110100100010010001010100101111100000110101100001011100100001000000111110010100"

