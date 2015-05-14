import Data.Char (ord, chr)

map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

-- Here we start on the String transmission section
type Bit = Int

bit2int :: [Bit] -> Int
bit2int bs = foldr (\x y -> x + 2*y) 0 bs

int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit n = n `mod` 2 : int2bit (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bs = take 8 (bs ++ repeat 0)

encode :: String -> [Bit]
encode = concat.map (make8.int2bit.ord) 

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bs = take 8 bs : chop8 (drop 8 bs)

decode :: [Bit] -> String
decode = map (chr.bit2int) . chop8

-- Here we start the homework
prob1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
prob1 f p xs = [f x | x <- xs, p x]

prob2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
prob2 f p xs = map f (filter p xs)

all1 :: (a -> Bool) -> [a] -> Bool
all1 p xs = and (map p xs)

any1 :: (a -> Bool) -> [a] -> Bool
any1 p xs = or (map p xs)

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x:xs) = if p x then x : takeWhile p xs else []

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs) = if p x then dropWhile1 p xs else (x:xs)

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x xs -> f x : xs) []

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\x xs -> if p x then x:xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\d x -> x + 10*d) 0

sumsqreven = sum.map (^2).filter even

f x y = x+y

curry1 :: ((a,b) -> c) -> a -> b -> c
curry1 f = \x y -> f (x,y)

uncurry1 :: (a -> b -> c) -> (a,b) -> c
uncurry1 f = \(x,y) -> f x y

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (null) (take 8) (drop 8)

map3 :: (a -> b) -> [a] -> [b]
map3 f = unfold (null) (\xs -> f (head xs)) tail

iterate1 :: (a -> a) -> a -> [a]
iterate1 f = unfold (\x->False) id f

-- Here we modify the String transmission program
addparity :: [Bit] -> [Bit]
addparity bs = (sum bs) `mod` 2 : bs

checkparity :: [Bit] -> [Bit]
checkparity bs = if (sum bs) `mod` 2 == 0 then tail bs else error "Parity check failed..." bs 

encodeP :: String -> [Bit]
encodeP = concat.map (addparity.make8.int2bit.ord) 

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bs = take 9 bs : chop9 (drop 9 bs)

decodeP :: [Bit] -> String
decodeP = map (chr.bit2int.checkparity) .chop9

channel :: [Bit] -> [Bit]
channel = id

badchannel :: [Bit] -> [Bit]
badchannel s = tail s

