import Data.Char
--prva kategoria
--1--

numbersCount xs = length [x|x<-xs,x `elem` ['0'..'9']]

--6--
isPrime x = (length [y | y <- [1..x], x `mod` y == 0]) <=2
primeGenerator n = [x | x <- [1..n], isPrime x]







--9--
suma xs = foldl (+) 0 xs


splitt (x:xs)	| (x==0) = Nothing
		| otherwise x : split xs

--ked posplitujem zoznam toto pouzijem na sucet
--main = map (foldl (+) 0) [[1, 2], [3, 4]]


tester xs = [x | x <- xs, x == 0]
foo xs = map (foldl (+) 0 xs)
foox xs = foldl (+) 0 xs
-- [foo x | x <-xs, x == 0]
--split xs = [[i] | i <- [0..(length xs - 1)], xs !! i /= 0]
bar i xs = foox (take i xs)
--split xs = [bar i xs | i <- [0..length xs -1], xs!!i == 0]


testing (x:y:xs) = (y:x:xs)










--druha kategoria
--2--
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

insert x [] = [x]
insert x (y:ys)	| (x < y) = (x:y:ys)
		| otherwise = y:(insert x ys)

--7--
type HashedPassword = Int
type PassDB = [(String, HashedPassword)]

users :: PassDB
users = [("jozo",294),("anton",300),("martin",305)]
-- toto je ok? aj ked ja vo funkcionalnom neviem zmenit zoznam akoze, spytat sa

hashPw xs = sum [ord(x) | x <- xs]
addUser (x,y) xs = (x,y) : xs

login :: PassDB -> String -> String -> Bool
--login xs x y	| (null [i | i <- xs, x == fst(i) && y == snd(i)]) = False
--		| otherwise = True
login xs x y = (x,hashPw(y)) `elem` xs

 