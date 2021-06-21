countNeg :: [Int] -> Int
countNeg xs = sum [1| x<-xs, x<0]



raise :: Int -> Int -> Int
raise a b = product (replicate b a)



pascal :: Int -> [Int]
pascal 0 = []
pascal 1 = [1]
pascal a = [1] ++ [x+y|(x,y)<-pairs (pascal (a-1))] ++ [1]

pairs :: [Int] -> [(Int, Int)]
pairs [a] = []
pairs x = zip (init x) (tail x)



q1f1a :: [Int] -> [Int]
q1f1a []= []
q1f1a x = map (*3) (filter (\b -> 3<=b&&b<=10) x)



q1f1b :: [Int] -> [Int]
q1f1b [] = []
q1f1b x = [a*3 | a <- x, a>=3&&a<=10]



compre :: [a] -> (a->b) -> (a->Bool) -> [b]
compre xs f p = map f (filter p xs)



subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x:xs) = (map (x:) (subsets xs)) ++ subsets xs



myMap :: (a->b) -> [a] -> [b]
myMap f [] = []
myMap f xs = foldr (\k ks -> (f k):ks) [] xs



occurrences :: [Char] -> [(Char,Int)]
occurrences xs = zip (remdup xs) [elemOcc x xs | x <- (remdup xs)]

elemOcc :: Char -> [Char] -> Int
elemOcc a = length . filter (==a)

remdup :: [Char] -> [Char]
remdup [] = []
remdup (x:xs) = x : remdup [k | k<-xs, k/=x]
