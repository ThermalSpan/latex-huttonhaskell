--Assoc
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k assoc = head [v | (k',v) <- assoc, k' == k]


--Some Practice with trees
data Tree a = Leaf a | Node a (Tree a) (Tree a)

t :: Tree Int
t = Node 4 (Node 2 (Leaf 1) (Leaf 3)) (Node 6 (Leaf 5) (Leaf 7))

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node a tl tr) = flatten tl ++ [a] ++ flatten tr

occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf b) = a == b
occurs a (Node b tl tr)                            
            | a == b    = True
            | a < b     = occurs a tl
            | otherwise = occurs a tr

-- Tautology Checker
type Subst = Assoc Char Bool

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

eval :: Subst -> Prop -> Bool
eval _ (Const a)     = a
eval s (Var c)       = find c s
eval s (Not p)       = not (eval s p)
eval s (And p1 p2)   = (eval s p1) && (eval s p2)
eval s (Imply p1 p2) = (eval s p1) <= (eval s p2)

findVars :: Prop -> [Char]
findVars (Const _)     = []
findVars (Var c)       = [c]
findVars (Not p)       = findVars p
findVars (And p1 p2)   = findVars p1 ++ findVars p2
findVars (Imply p1 p2) = findVars p1 ++ findVars p2

boolGen :: Int -> [[Bool]]
boolGen 0 = [[]]
boolGen n = map (False:) bs ++ map (True:) bs
                where bs = boolGen (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (boolGen (length vs))
           where vs = rmdups (findVars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

--Some sample propositions
p1 :: Prop
p1 = And (Var 'A') (Var 'B')

p2 :: Prop
p2 = And (Imply (Var 'A') (And (Var 'C') (Var 'B'))) (And (Var 'C')(Var 'A'))

p3 :: Prop
p3 = And (Not (Var 'A')) (Var 'A')

p4 :: Prop
p4 = Imply (Var 'A') (Var 'A')

--Abstract Machine Example
data Expr = Val Int
          | Add Expr Expr

value :: Expr -> Int
value (Val n)     = n
value (Add e1 e2) = value e1 + value e2

type Cont = [Op]
data Op = ADD Int
        | EVAL Expr

eval' :: Expr -> Cont -> Int
eval' (Val n) c   = exec c n
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n            = n
exec (ADD n : xs) m  = exec xs (n + m)
exec (EVAL y : xs) m = eval' y (ADD m : xs)

value' :: Expr -> Int
value' e = eval' e []

-- Some somple expressions
t1 :: Expr
t1 = Add (Add (Val 1) (Val 1)) (Add (Add (Val 2) (Val 3)) (Val 3))

t2 :: Expr
t2 = Add (Val 2) (Val 2)





