--Testing?
test1 :: IO ()
test1 = do
    x <- getLine
    case x of
      "a"       -> putStrLn "its an a!"
      "return"  -> test1
      "test"    -> let y = x ++ " test?" in putStrLn y
      otherwise -> putStrLn "Its not an a"

--Example functions
beep :: IO ()
beep = putStr "\BEL"

cls :: IO () 
cls = putStr "\ESC[2J"
   
type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt p s = do goto  p
                 puStr s

seqn :: [IO ()] -> IO ()
seqn [] = return ()
seqn (x:xs) = do x
                 seqn xs




