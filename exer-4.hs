data BExpr = F | T | Not BExpr
           | BExpr :&: BExpr
	   | BExpr :|: BExpr

eval :: BExpr -> Bool
eval F = False
eval T = True
eval (x :&: y) = eval x && eval y
eval (x :|: y) = eval x || eval y
eval (Not x) = not (eval x)



data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

bfs :: Tree a -> [a]
bfs x = traverse [x]

traverse :: [Tree a] -> [a]
traverse [] = []
traverse ts = rootlabels ++ traverse children
   where rootlabels = [x | (Node x _ _) <- ts]
         children = concat [[l,n]|(Node _ l n)<- ts]


data Edit = Change Char | Copy | Delete | Insert Char deriving (Eq, Show)

cost :: [Edit] -> Int
cost = length.filter (/=Copy)

transform :: String -> String -> [Edit]
transform [] [] = []
transform st [] = replicate (length st) Delete
transform [] st = map Insert st
transform (a:x) (b:y)
      | a==b = Copy : transform x y
      | otherwise = best [Delete : transform x (b:y),
	                  Insert b : transform (a:x) y,
			  Change b : transform x y]

best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
    | cost x <= cost b = x
    |otherwise = b
       where b = best xs
