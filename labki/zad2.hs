import Char

--zad1
single:: [a] -> [[a]]
single [x] =[[x]]
single (x:xs) =
    [x]:(single xs)

--zad2
mymap f xs = [f x | x <- xs]
myfilter f xs = [x| x <- xs, f x ]

--zad3
allTrue :: [Bool] -> Bool
allTrue x = foldr (==) True x

--zad4
data Day = Mon | Tue | Wed | Thu | Fri |
           Sat | Sun deriving (Show, Enum)

whichDay :: Day -> Int -> Day
whichDay d i = toEnum(((fromEnum d) + i - 1) `mod` 7)

--zad5
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

interNodes :: Tree a -> [a]
interNodes Empty = []
interNodes (Node _ Empty Empty) = []
interNodes (Node x l Empty) = [x] ++ interNodes l
interNodes (Node x Empty r) = [x] ++ interNodes r
interNodes (Node x l r) = [x] ++ interNodes l ++ interNodes r

t = Node 5 (Node 3 (Node 8 Empty Empty) (Node 1 Empty Empty))
      	                (Node 4 Empty (Node 6 Empty Empty))
mirrorTree :: Tree a -> Tree a
mirrorTree Empty = Empty
mirrorTree (Node x l r) = Node x (mirrorTree r) (mirrorTree l)


nodesAtLevel :: Tree a -> Int -> [a]
nodesAtLevel (Node x l r) 0 = [x]
nodesAtLevel (Node x l r) i = (nodesAtLevel l (i-1)) ++ (nodesAtLevel r (i-1))
nodesAtLevel Empty _ = []

--zad6
--data FSObject = Katalog String [FSObject] | Plik String deriving (Show)
--f = Katalog "root" [Katalog "usr" [Plik "porn.jpg", Plik "lolsy.jpg"], Plik ".bashrc"]

--data FSPath = String String deriving (Show)

--search :: String ->  String -> FSObject -> Maybe String
--search q t (Plik n) = if q==n then t ++ "/" ++ n else Nothing
--search q t (Katalog n (x:xs)) = map (\x -> search(q "/" ++ n x))

