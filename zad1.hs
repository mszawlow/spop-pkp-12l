import Char

--zad 1
middle :: [a] -> a
middle a = a !! div (length a) 2 

--zad2 
removeDups :: Eq a => [a] -> [a]
removeDups (x:y:xs) = 
	if x == y 
		then removeDups (y:xs) else x:removeDups (y:xs)
removeDups [x] = [x]
removeDups [] = []

--zad3
removeDups2 :: Eq a => [a] -> [a]
inList x [] = False
inList x xs = x == xs !! 0 || inList x (tail xs)

removeDups2 [x] = [x]
removeDups2 [] = []
removeDups2 (x) = if inList (last x) (init x) then (removeDups2 (init x)) else (removeDups2 (init x))++[last(x)]

--zad4
adjpairs [] = []
adjpairs [a] = []
adjpairs (x:y:xs) = (x,y) : adjpairs (y:xs)		

--zad5
string2int :: String -> Int
string2int [x] = digitToInt x
string2int (x:xs) = if (x == '-') && not (isDigit x) then -1 * string2int xs else 10^(length xs)*digitToInt x + string2int xs

--zad6
insertAt :: [a] -> a -> Int -> [a]
insertAt (x:xs) y z = if z == 0 
	then (y:x:xs)
	 else (x:insertAt xs y (z-1))

--zad7
--codeCezar :: String -> Int -> String
--ord
--chr


