import Data.Char

--2.	Definujte funkciu insertSort pre usporiadanie prvkov zoznamu priamym vkladan�m (insert sort).

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)	| (x < y) = (x:y:ys)
		| otherwise = y:(insert x ys)



--7.	De?nujte funkciu, ktor� realizuje prihlasovanie pou��vate�a do syst�mu. Nech
--	funkcia ako parametre prij�ma datab�zu pou��vate�ov, meno a heslo. Hesl� s�
--	v datab�ze ukladan� v �ifrovanej podobe.
--	De?nujte tie� funkciu pre pridanie pou��vate�a do datab�zy
	

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
