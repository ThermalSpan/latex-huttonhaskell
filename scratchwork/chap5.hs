import Data.Char (ord, chr, isLower, isUpper, toLower)

concat :: [[a]]->[a]
concat xss = [x | xs<-xss, x<-xs]

factors :: Int->[Int]
factors n = [x | x<-[1..n], n`mod`x == 0]

prime :: Int->Bool
prime n = factors n == [1, n]

primes :: Int->[Int]
primes n = [p | p<-[1..n], prime p]

squares :: Int -> [Int]
squares n = [ a^2 | a<-[1..n]]

tuples :: Int -> [(Int,Int,Int)]
tuples n = [ (x,y,z) | x<-[1..n], y<-[1..x], z<-[1..y]]

tuptolist :: [(Int,Int,Int)] -> [[Int]]
tuptolist ts = [ [x,y,z] | (x,y,z)<-ts]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [ x<=y | (x,y)<-pairs xs]

count :: Char->String->Int
count c s = length [ c' | c'<-s, c==c']

lowers :: String -> Int
lowers s = length [ c | c<-s, isLower c]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i | (x', i) <- zip xs [1..], x'==x]

-- Here we begin the 'Ceasar Cipher' section of the chapter
let2int :: Char->Int
let2int c | isLower c = ord c - ord 'a'
          | isUpper c = let2int (toLower c)

int2let :: Int->Char
int2let i = chr( i + ord 'a')

shift :: Int->Char->Char
shift n c | isUpper c || isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int->String->String
encode n s = [shift n c | c<-s]

-- Here we work on cracking the cipher
table :: [ Float ]
table = [ 8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1 ]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100.0

freq :: String -> [Float]
freq s = [percent (count c s) n | c<-['a'..'z']]
          where n = lowers s

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[ (o - e)^2 | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack s = encode (-factor) s
           where
             factor = head(positions (minimum chitab) chitab)
             chitab = [chisqr (rotate n table') table | n <- [1..25]]
             table' = freq s

-- Here we start on the Problems
replicate :: Int -> a -> [a]
replicate n x = [ x | _<-[1..n]]

isSquare :: Int -> Bool
isSquare n = s * s == n
             where
               s = truncate (sqrt x)
               x = fromIntegral n

pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x,y,z) | x<-[1..n], y<-[1..n], 
            let s = x^2 + y^2, 
            let z = truncate (sqrt (fromIntegral s)), 
            z*z==s, z <= n]

perfects1 :: Int -> [Int]
perfects1 n = [i | i <- [1..n], sum (factors i) - i == i]

perfects2 :: Int -> [Int]
perfects2 n = [i | i <- [1..n], sum (filter (\x->x/=i) (factors i)) == i]

find :: Eq a => a -> [(a,b)] -> [b]
find k as = [v | (k', v) <- as, k'==k]

positions2 :: Eq a => a -> [a] -> [Int]
positions2 a as = find a (zip as [1..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

--Modifying the Ceasar Cipher





