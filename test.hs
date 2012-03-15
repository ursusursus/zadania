xs = [1,2,3,0,4,5,6,7,0,8,9]
--bar [] = []
--bar (x:xs)	| (x == 0) = dropWhile (/=0) xs
--		| otherwise = takeWhile (/=0) (x:xs)
bar xs = dropWhile (/=0) (tail (dropWhile (/=0) xs))
--treba najprv tejknem, to iste dropnem, a potom to dropnute takenem atd
--striedat take, drop ak parny pocet ntic, tak posledne musi byt sum .. asi

foo x = tail (dropWhile (/=0) xs)