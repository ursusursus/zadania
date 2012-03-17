-- opravit typovu definiciu v jednotke

--1.	De?nujte funkciu numbersCount pre zistenie, poètu èíslic v reazci.

--numbersCount :: Num a => [Char] -> a
numbersCount xs = length [ x | x <- xs, x `elem` ['0'..'9']]

--6.	De?nujte funkciu primeGenerator jedného argumentu n, ktorá vygeneruje
--	zoznam prvých n prvoèísel.

isPrime :: Int -> Bool
isPrime x = (length [ y | y <- [1..x], x `mod` y == 0]) <=2

primeGenerator :: Int -> [Int]
primeGenerator n = [ x | x <- [1..n], isPrime x]


--9.	Na vstupe nech je zoznam kladných èísel rozdelených do skupín oddelených
--	èíslom 0. Napr. zoznam [1; 2; 5; 0; 3; 6; 8; 2] obsahuje dve skupiny [1; 2; 5] a
--	[3; 6; 8; 2].
--	De?nujte funkciu, ktorá z takého zoznamu vypoèíta zoznam obsahujúci súèty
--	èísel v každej skupine (pre uvedený príklad bude výsledok [8; 19]). Použite
--	funkciu foldl.

