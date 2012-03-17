import Data.Char

--2.	Definujte funkciu insertSort pre usporiadanie prvkov zoznamu priamym vkladaním (insert sort).

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)	| (x < y) = (x:y:ys)
		| otherwise = y:(insert x ys)



--7.	De?nujte funkciu, ktorá realizuje prihlasovanie používate¾a do systému. Nech
--	funkcia ako parametre prijíma databázu používate¾ov, meno a heslo. Heslá sú
--	v databáze ukladané v šifrovanej podobe.
--	De?nujte tiež funkciu pre pridanie používate¾a do databázy
	

type HashedPassword = Int
type PassDB = [(String, HashedPassword)]

users :: PassDB
users = [("jozo",294),("anton",300),("martin",305)]

hashPassword :: String -> Int
hashPassword xs = sum [ord(x) | x <- xs]

addUser :: (String,String) -> PassDB -> PassDB
addUser (x,y) xs = (x,hashPassword y) : xs

login :: PassDB -> String -> String -> Bool
login xs x y = (x,hashPassword y) `elem` xs
