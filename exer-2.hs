power1 :: Int -> Int -> Int
power1 n k | k<0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product (replicate k n)


power2 :: Int->Int->Int
power2 n k | k<0 = error "power: negative argument"
           | k==0  = 1
           | even k = power2 (n*n) (k `div` 2)
	   | odd k = n * power2 n (k-1)


myButLast :: [a]->a
myButLast [i,j] = i
myButLast (x:xs) = myButLast xs


rev2 :: [a]->[a]
rev2 []=[]
rev2 [i,j]=[j,i]
rev2 x=x


upto :: Int->Int->[Int]
upto m n = tailUpto m n []

tailUpto :: Int->Int->[Int]->[Int]
tailUpto n k x | n>k = x
                 | n<=k = tailUpto n (k-1) (k:x)

fib :: Int->Int
fib n = tailFib n 0 1

tailFib :: Int->Int->Int->Int
tailFib i pre res | i<0 = error "negative argument"
               | i==0 = 0
               | i==1 = res
	       | otherwise = tailFib (i-1) res (pre+res)


palindrome :: [Int]->Bool
palindrome [] = True
palindrome [a] = True
palindrome (x:xs) | x==(last xs) = palindrome (init xs)
                  | otherwise = False


removeOnce :: Int->[Int]->[Int]
removeOnce i [] = []
removeOnce i (x:xs) | i==x = xs
                    | otherwise = x:(removeOnce i xs)


isPermutation :: [Int]->[Int]->Bool
isPermutation [] [] = True
isPermutation i j | null i||null j = False
                  | otherwise = isPermutation (tail i) (removeOnce (head i) j)
