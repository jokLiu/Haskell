import System.IO


--ex1
putStr2 :: String -> IO ()
putStr2 xs = sequence_ [putChar x | x <- xs]

--ex2
type Board = [Int]

putBoard :: Board -> IO ()
putBoard brd = putBrd brd 1

putBrd :: Board -> Int -> IO ()
putBrd []     rw = return ()
putBrd (b:bd) rw = do putStr (show rw) 
                      putStr ": "
                      putStrLn (concat (replicate b "* "))
                      putBrd bd (rw+1)

--ex3
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row) 
                    putStr ": "
                    putStrLn (concat (replicate num "* "))
                      
putBoard2 :: Board -> IO ()
putBoard2 bd = sequence_ [putRow rw b | (rw,b) <- zip [1..] bd]


--ex4
adder :: IO ()
adder = do putStr "How many numbers? "
           xs <- getLine
           let zs = read xs :: Int
           adderHelp zs 0

adderHelp :: Int -> Int -> IO ()
adderHelp num ttl = do x <- getLine
                       let n = read x :: Int
                       if num <= 1 
                       	   then putStrLn ("The total is " ++ (show (ttl+n)))
                       	   else adderHelp (num-1) (ttl+n)



--ex6


getCh :: IO Char 
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x



readLine :: IO String
readLine = readLi []

readLi  :: String -> IO String
readLi ls= do x <- getCh
              if x == '\DEL' then
              	 do putChar '\b'
              	    putChar ' '
              	    putChar '\b'
              	    readLi (take ((length ls)-1) ls)
              else if x == '\n' then
              	 do putChar x
              	    return (ls)
              else 
              	 do putChar x
              	    readLi (ls++[x])
            