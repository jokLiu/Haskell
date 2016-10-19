import Huffman
str :: String
str = "bp''mmooodddjjjaaaaiiiirrrrruuuuullllllsssssseeeeeeee            "

str2 = "labas"
--test1 :: Bool
test1 = buildTableMain (buildTree (reverse (toHuffList (mergesort (freq str)))))