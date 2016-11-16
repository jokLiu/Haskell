{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP #-}
module TestBench where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad
import System.Timeout
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess, output)
import Data.IORef

-- Data follows.

import safe Assessed1Part2 (
    decode, decompress, decompress', charlength, memSize, Tree(..), Bit(..), tabulate, makeTree, generateTree, makeTable, encodeUsingTable, encodeUsing, encode, compress, compress', CodingTable, Freq, Key, freq,
    )
import TestHelpers


-- This file was automatically generated
-- by GenerateTests.hs
-- on 2016-11-02 16:11:55.337505025 UTC

main :: IO ()
main = do
    points <- newIORef 0

    putStrLn "Test 1: makeTree -- correctness (3 points)"
    res_1_1 <- unitTests "test_makeTree_correctness" test_makeTree_correctness test_maketree_correctness_1_1_in test_maketree_correctness_1_1_out
    addPoints points $ defaultMarker "makeTree -- correctness" 3 [res_1_1]
    putStrLn ""

    putStrLn "Test 2: makeTree -- optimality (2 points)"
    res_2_1 <- unitTests "test_makeTree_optimality" test_makeTree_optimality test_maketree_optimality_2_1_in test_maketree_optimality_2_1_out
    addPoints points $ defaultMarker "makeTree -- optimality" 2 [res_2_1]
    putStrLn ""

    putStrLn "Test 3: generateTree (3 points)"
    res_3_1 <- unitTests "test_generateTree_correctness" test_generateTree_correctness test_generatetree_correctness_3_1_in test_generatetree_correctness_3_1_out
    addPoints points $ defaultMarker "generateTree" 3 [res_3_1]
    putStrLn ""

    putStrLn "Test 4: makeTable (3 points)"
    res_4_1 <- unitTests "test_makeTable" test_makeTable test_maketable_4_1_in test_maketable_4_1_out
    addPoints points $ defaultMarker "makeTable" 3 [res_4_1]
    putStrLn ""

    putStrLn "Test 5: encodeUsingTable (3 points)"
    res_5_1 <- unitTests "test_encodeUsingTable" test_encodeUsingTable test_encodeusingtable_5_1_in test_encodeusingtable_5_1_out
    addPoints points $ defaultMarker "encodeUsingTable" 3 [res_5_1]
    putStrLn ""

    putStrLn "Test 6: encodeUsing (3 points)"
    res_6_1 <- unitTests "test_encodeUsing" test_encodeUsing test_encodeusing_6_1_in test_encodeusing_6_1_out
    addPoints points $ defaultMarker "encodeUsing" 3 [res_6_1]
    putStrLn ""

    putStrLn "Test 7: encode (3 points)"
    res_7_1 <- unitTests "test_encode_correctness" test_encode_correctness test_encode_correctness_7_1_in test_encode_correctness_7_1_out
    addPoints points $ defaultMarker "encode" 3 [res_7_1]
    putStrLn ""

    putStrLn "Test 8: compress (3 points)"
    res_8_1 <- unitTests "test_compress_correctness" test_compress_correctness test_compress_correctness_8_1_in test_compress_correctness_8_1_out
    addPoints points $ defaultMarker "compress" 3 [res_8_1]
    putStrLn ""

    putStrLn "Test 9: compress' (2 points)"
    res_9_1 <- unitTests "test_compress'_1" test_compress'_1 test_compress'_1_9_1_in test_compress'_1_9_1_out
    res_9_2 <- unitTests "test_compress'_2_correctness" test_compress'_2_correctness test_compress'_2_correctness_9_2_in test_compress'_2_correctness_9_2_out
    addPoints points $ defaultMarker "compress'" 2 [res_9_1, res_9_2]
    putStrLn ""
    putStrLn "Total possible points for this set: 25"
    putStrLn "Expected number of points for your solution: "
    print =<< readIORef points
    putStrLn "Your expected mark is: "
    print . (/ 0.25) . fromIntegral =<< readIORef points

-- START automatically generated inputs/outputs --


-- Inputs and correct outputs for test_makeTree_correctness
test_maketree_correctness_1_1_in = [
    -- test_maketree_correctness_1_1_in !! 0
    ()
  ]
test_maketree_correctness_1_1_out = [
    -- test_maketree_correctness_1_1_out !! 0
    True
  ]

-- Inputs and correct outputs for test_makeTree_optimality
test_maketree_optimality_2_1_in = [
    -- test_maketree_optimality_2_1_in !! 0
    ()
  ]
test_maketree_optimality_2_1_out = [
    -- test_maketree_optimality_2_1_out !! 0
    True
  ]

-- Inputs and correct outputs for test_generateTree_correctness
test_generatetree_correctness_3_1_in = [
    -- test_generatetree_correctness_3_1_in !! 0
    ()
  ]
test_generatetree_correctness_3_1_out = [
    -- test_generatetree_correctness_3_1_out !! 0
    True
  ]

-- Inputs and correct outputs for test_makeTable
test_maketable_4_1_in = [
    -- test_maketable_4_1_in !! 0
    ()
  ]
test_maketable_4_1_out = [
    -- test_maketable_4_1_out !! 0
    True
  ]

-- Inputs and correct outputs for test_encodeUsingTable
test_encodeusingtable_5_1_in = [
    -- test_encodeusingtable_5_1_in !! 0
    ()
  ]
test_encodeusingtable_5_1_out = [
    -- test_encodeusingtable_5_1_out !! 0
    True
  ]

-- Inputs and correct outputs for test_encodeUsing
test_encodeusing_6_1_in = [
    -- test_encodeusing_6_1_in !! 0
    ()
  ]
test_encodeusing_6_1_out = [
    -- test_encodeusing_6_1_out !! 0
    True
  ]

-- Inputs and correct outputs for test_encode_correctness
test_encode_correctness_7_1_in = [
    -- test_encode_correctness_7_1_in !! 0
    ()
  ]
test_encode_correctness_7_1_out = [
    -- test_encode_correctness_7_1_out !! 0
    True
  ]

-- Inputs and correct outputs for test_compress_correctness
test_compress_correctness_8_1_in = [
    -- test_compress_correctness_8_1_in !! 0
    ()
  ]
test_compress_correctness_8_1_out = [
    -- test_compress_correctness_8_1_out !! 0
    True
  ]

-- Inputs and correct outputs for test_compress'_1
test_compress'_1_9_1_in = [
    -- test_compress'_1_9_1_in !! 0
    ()
  ]
test_compress'_1_9_1_out = [
    -- test_compress'_1_9_1_out !! 0
    True
  ]

-- Inputs and correct outputs for test_compress'_2_correctness
test_compress'_2_correctness_9_2_in = [
    -- test_compress'_2_correctness_9_2_in !! 0
    ()
  ]
test_compress'_2_correctness_9_2_out = [
    -- test_compress'_2_correctness_9_2_out !! 0
    True
  ]


-- Common part follows.

timeOutTime :: Num a => a
timeOutTime = 10^6 * 60 -- One minute

unitTest :: (Eq b, Show a, Show b) => String -> (a -> b) -> a -> b -> IO Bool
unitTest name f x spec = do
  -- Using QuickCheck here for convenience (since it catches exceptions and
  -- timeouts for us)
  result <- quickCheckWithResult stdArgs {chatty = False}
          $ timeoutProp (f x == spec)

  case result of
    Failure {} -> do
      putStrLn "*** Failed!"
      putStrLn $ "Input: " ++ show x
      catch (timeoutOutput $ putStrLn $ "Output: " ++ show (f x))
            (\msg -> putStrLn $ "Exception: " ++ show (msg :: SomeException))
      putStrLn $ "Expected output: " ++ show spec
      return False

    GaveUp {} -> do
      putStrLn "Gave up."
      return False
    _ -> return True

  where
    timeoutOutput :: IO a -> IO a
    timeoutOutput action = do
        res <- timeout timeOutTime action

        case res of
            Just result -> return result
            Nothing     -> error "<<timeout>>"

unitTests :: (Eq b, Show a, Show b) => String -> (a -> b) -> [a] -> [b] -> IO Float
unitTests name f inputs spec = do
  putStr $ "  [testing] " ++ name ++ "... "
  res <- run $ zipWith (unitTest name f) inputs spec

  when (res == 1) $ putStrLn "ok."

  return res

  where
    run [] = return 1
    run (x:xs) = do
      r <- x
      if r then run xs
      else return 0

quickTest :: Testable prop => String -> prop -> IO Bool
quickTest name = quickTestWith name stdArgs

quickTestWith :: Testable prop => String -> Args -> prop -> IO Bool
quickTestWith name args0 prop = do
  putStr $ "  [checking] " ++ name ++ "... "
  let args = args0 {chatty = False}
  result <- quickCheckWithResult args
          $ timeoutProp prop

  if not (isSuccess result)
    then do
        putStrLn "eh, nope."
        putStrLn (output result)
        return False
    else do
        putStrLn "ok."
        return True

timeoutProp :: Testable prop => prop -> Property
timeoutProp = within timeOutTime

-- Gives full mark only if all parts of the exercise succeded. Otherwise gives
-- 0 points.
defaultMarker :: String -> Int -> [Float] -> Int
defaultMarker exerciseName points bs
    -- There is only a single floating-point representation for 1, so this is safe.
    | all (==1) bs    = points
    | otherwise = 0

addPoints :: IORef Int -> Int -> IO ()
addPoints var points = putStrLn ("\nPoints given: " ++ show points) >> modifyIORef' var (+ points)
