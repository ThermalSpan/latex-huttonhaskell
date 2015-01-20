n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

last1 xs = xs!!((length xs)-1)

last2 (x:[]) = x
last2 (x:xs) = last xs

last3 xs = head (reverse xs)

init1 xs = take ((length xs)-1) xs 

init2 xs = reverse (tail (reverse xs))

