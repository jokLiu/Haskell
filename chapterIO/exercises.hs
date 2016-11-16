--ex1
putStrr :: String -> IO ()
putStrr xs = sequence_ [ putChar x | x <- xs] 


type Board = [Int]

putBoard :: Board -> IO ()
