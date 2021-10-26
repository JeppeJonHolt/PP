main = do
    print (fib 4)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse'(xs) ++ [x]

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1)+fib(n-2)

infFib :: Integer -> Integer -> [Integer]
infFib n k = (n+k) : infFib k (n+k)

sequence' :: Integer -> Integer -> [Integer]
sequence' n k = n : (sequence' (n+k) k)


fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact(n-1)

addition :: Integer -> Integer -> Integer
addition a b = a+b

palindrom :: (Eq a) => [a] -> Bool
palindrom (x) = x == reverse' (x)

last' :: [a] -> a
last' [a] = a
last' (x:xs) = last' xs

flatten' :: [[a]] -> [a]
flatten' [] = []
flatten' (x:xs) = x ++ flatten'(xs)

isPrime :: Integer -> Bool
isPrime n = devisor n == [1,n]

devisor :: Integer -> [Integer]
devisor n = [i | i <-[1..n], n `mod` i == 0] 

data Nat = Zero | Succ Nat deriving Show
natrualNum = Succ ( Succ ( Succ ( Zero )))

nat2int :: Nat -> Integer
nat2int Zero = 0
nat2int (Succ n) = nat2int n + 1

int2nat :: Integer -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

data Tree a = Leaf a| Node (Tree a) (Tree a)

--isBalanced :: Node -> Bool
isBalanced (Node t1 t2) = c1 || c2 || c3
    where c1 = getDepth(t1) == getDepth(t2)-1
          c2 = getDepth(t1)-1 == getDepth(t2)
          c3 = getDepth(t1) == getDepth(t2)

--getDepth :: Tree -> Integer
getDepth (Leaf n) = 0
getDepth (Node t1 t2) = ((getDepth t1)+ 1) + ((getDepth t2) + 1)

myBalancedTree = Node (Node (Leaf(1)) (Leaf(1))) (Node (Leaf(1))(Leaf(1)))
myUnBalancedTree = Node (Node (Node (Leaf (1))(Leaf (1))) (Leaf(1))) (Node (Leaf(1))(Leaf(1)))

div3 = [3*i | i <- [0..]]

perfectSquare n = take n[i*i|i <-[0..]]
perfectSquare' n = [i*i|i <-[0..n]]


data BExp = TT | FF | Or BExp BExp | And BExp BExp | Neg BExp
mybexp = And (Or TT TT) (Or FF (And FF TT))

eval' TT = True
eval' FF = False
eval' (Or b1 b2) = v1 || v2 -- eval b1 || eval b2
                    where v1 = eval' b1
                          v2 = eval' b2
eval' (And b1 b2) = v1 && v2
                    where v1 = eval' b1
                          v2 = eval' b2
eval' (Neg b) = not v 
                where v = eval' b