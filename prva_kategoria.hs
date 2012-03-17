-- opravit typovu definiciu v jednotke

--1.	Definujte funkciu numbersCount pre zistenie, po�tu ��slic v re�azci.

numbersCount :: Num a => [Char] -> a
numbersCount xs = fromIntegral (length [ x | x <- xs, x `elem` ['0'..'9']])

--6.	Definujte funkciu primeGenerator jedn�ho argumentu n, ktor� vygeneruje
--	zoznam prv�ch n prvo��sel.

isPrime :: Int -> Bool
isPrime x = (length [ y | y <- [1..x], x `mod` y == 0]) <=2

primeGenerator :: Int -> [Int]
primeGenerator n = [ x | x <- [1..n], isPrime x]


--9.	Na vstupe nech je zoznam kladn�ch ��sel rozdelen�ch do skup�n oddelen�ch
--	��slom 0. Napr. zoznam [1; 2; 5; 0; 3; 6; 8; 2] obsahuje dve skupiny [1; 2; 5] a
--	[3; 6; 8; 2].
--	Definujte funkciu, ktor� z tak�ho zoznamu vypo��ta zoznam obsahuj�ci s��ty
--	��sel v ka�dej skupine (pre uveden� pr�klad bude v�sledok [8; 19]). Pou�ite
--	funkciu foldl.

count :: [Int] -> Int
count xs = foldl (+) 0 xs

input [] = []
input xs = count (takeWhile (/= 0) xs) : input (drop n xs) 
		where n = length (takeWhile (/=0) xs) + 1


