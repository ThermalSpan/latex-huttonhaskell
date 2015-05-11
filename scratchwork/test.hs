test  :: Int -> Int
test n = n + 2

squares :: Int -> [Int]
squares n = [ a^2 | a<-[1..n]]

tuples :: Int -> [(Int,Int,Int)]
tuples n = [ (x,y,z) | x <- [1..n], y<-[1..x], z<-[1..y]]

stringtest s = [ c | c<-s]
