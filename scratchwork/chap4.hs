halve1 xs | even (length xs) = (take n xs, drop n xs) where n = (length xs) `div` 2

safetail1 xs = if xs == [] then [] else tail where (_:tail) = xs
safetail2 xs | xs == []  = []
             | otherwise = tail where (_:tail)=xs
safetail3 [] = []
safetail3 (x:xs) = xs


