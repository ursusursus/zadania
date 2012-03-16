--3.kat -- 4uloha
--nefunguje dobre lebo velke cislo je to uz a zle to premeni..
data Date = Date Int Int Int
data Sex = Male | Female deriving (Show,Eq)

foo d m y = y ++ m ++ d

--base = read (foo "07" "09" "90") :: Int
--base = foo "07" "09" "90" ++ "1000"

base = (read (foo "07" "09" "90") :: Int) * 1000

findFirst x	| (x `mod` 11 == 0) = x
		| otherwise = findFirst (succ(x))

quax = [findFirst base + (11 * i) | i <- [0..n]] where n = 1000 `div` 11

format x = take 6 (show x) ++ "/" ++ drop 6 (show x) ++ "\n"
output = putStrLn (concat (map format quax))