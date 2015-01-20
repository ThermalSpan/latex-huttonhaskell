abs1 n = if n >= 0 then n else -n

sign1 n = if n > 0 then 1 else
          if n ==0 then 0 else -1

fooguard n | n > 0, even n = 1
           | n < 0, even n = -1
           | otherwise     = 0

test1 ((_:xs),(_:ys)) = (xs, ys)
test1 ([],[])         = (0,0)

f = \x -> x+x

