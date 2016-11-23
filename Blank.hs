{-# LANGUAGE CPP #-}

module Blank where

import Control.Monad
import Control.DeepSeq
import Data.Char
import Data.List
import Data.Maybe
--import System.Console.Readline
import System.IO.Unsafe
import System.Random

import Control.Monad.State
import Data.Either



type Dict = [String]
type Board = [[Maybe Char]]
type Rack = String
type Pos = (Int, Int)
data Orient = H | V deriving (Show, Eq, Read)
type WordPos = (Pos, Orient)
type Move = (String, WordPos)
type Score = Int
data PlayError = NoFitOnBoard | NotOnRack | NotAWord deriving (Show)
type Seed = StdGen

newtype LRand a = LRand (Seed -> a)

instance Functor LRand where
 fmap f (LRand h) = LRand (f.h)

instance Applicative LRand where
 pure  = return
 (<*>) = ap

instance Monad LRand where
 return x = LRand (\seed -> x)  -- The seed is ignored.

 LRand m >>= k =                -- The seed is not only used, but also transformed and propagated.
   LRand (\s ->
     let (s1,s2)  = split s     -- The split function is predefined in the random libraries. Hoogle it.
         LRand m' = k (m s1)
      in m' s2
   )


data Input = MoveInput Move | Exit | NewTiles
type Template = (Char, WordPos, Int, Int)
type LetterStream = [Char]
type FullMove = Maybe Move
type AI = Board -> LetterStream -> [FullMove] -> [FullMove]



boardFromWord :: String -> Board
boardFromWord xs = undefined


numOcc :: Char -> String -> Int
numOcc = undefined
submultiset :: String -> String -> Maybe String
submultiset = undefined
formable :: String -> Rack -> Char -> Maybe String
formable = undefined
wordValue :: String -> Score
wordValue = undefined
invertBoard :: Board -> Board
invertBoard = undefined
autoResize :: Board -> Board
autoResize = undefined
writeMove :: Move -> Board -> Board
writeMove = undefined
newLetter :: LRand Char
newLetter = undefined
replenishRack :: Rack -> LRand Rack
replenishRack = undefined
allWords1 :: Dict -> Char -> [String]
allWords1 = undefined
allWords2 :: Dict -> Char -> Int -> Int -> [(String, Int)]
allWords2 = undefined
allWords3 :: Dict -> Rack -> Char -> Int -> Int -> [(String, Int)]
allWords3 = undefined

greedyBestMove :: Dict -> Rack -> Board -> FullMove
-- We use read and show to convert between Bram.FullMove and
-- Scrabble.FullMove.
greedyBestMove dict rack b = undefined

aiOneMove :: Dict -> State (Board, Rack, LetterStream, [FullMove]) FullMove
aiOneMove dict = undefined


ai :: Dict -> AI
ai dict = undefined

{-
convertAIfromBram :: Bram.AI -> AI
convertAIfromBram ai board stream
    = undefined
convertAItoBram :: AI -> Bram.AI
convertAItoBram ai board stream
    = undefined
    -}