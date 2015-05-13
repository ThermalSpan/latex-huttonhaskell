and1 :: [Bool] -> Bool
and1 [] = True
and1 (False:_) = False
and1 (True :xs) = and xs

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x++(concat1 xs)

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n a = a:(replicate (n-1) a)

(+*) :: [a] -> Int -> a
(x:xs) +* 0 = x
(x:xs) +* n = xs +* (n-1)

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 a (x:xs) = if a==x then True else elem1 a xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) = if x <= y then x:(merge xs (y:ys)) else y:(merge ys (x:xs)) 

halve :: [a] -> ([a],[a])
halve xs = (take h xs, drop h xs) 
            where 
                h = (length xs) `div` 2

mergesort :: Ord a => [a] -> [a]
mergesort [] =[]
mergesort [a] = [a]
mergesort xs = merge (mergesort h1) (mergesort h2) where (h1,h2) = halve xs

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

take1 :: Int -> [a] -> [a]
take1 0 _ = []
take1 n (x:xs) = x : (take1 (n-1) xs)

last1 :: [a] -> a
last1 [a] = a
last1 (x:xs) = last xs
