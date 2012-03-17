--4.	Vytvorte funkciu, ktorá bude generova všetky platné rodné èísla pre zadaný
--	dátum narodenia a pohlavie. Na reprezentáciu dátumu a pohlavia použite
--	dátové typy.
--	Vygenerované rodné èísla preveïte na reazce v tvare "xxxxxx=xxxx", prièom
--	platí, že rodné èíslo je platné vtedy, ak je delite¾né èíslom 11.

--neplatne datumy vyhodit error .. ?
--data Date = Date [1954..] [1..12] [1..31] deriving (Show)
data Date = Date Int Int Int deriving (Show) -- Year Month Day
data Sex = Male | Female deriving (Eq)


initialize :: Date -> Sex -> Date
initialize (Date y m d) s	| (s == Female) = (Date y (m+50) d)
			 	| otherwise = (Date y m d)

findFirstPostfix :: Date -> Int -> Int
findFirstPostfix (Date y m d) postfix 	| ((y+m+d+postfix) `mod` 11 == 0) = postfix
					| otherwise = findFirstPostfix (Date y m d) (succ postfix)

findAllPostfixes :: Int -> [Int]
findAllPostfixes p	| (p < 10000) = p : findAllPostfixes (p+11)
			| otherwise = []


format :: Date -> Int -> String
format (Date y m d) postfix = (format' y 2) ++ (format' m 2) ++ (format' d 2) ++ "/" ++ (format' postfix 4) ++ "\n"

format' :: Int -> Int -> String
format' number expectedDigits = zeros ++ show number
	where	zeros = [ '0' | _ <- [1..n]]
		n = expectedDigits - length (show number)


generate :: Date -> Sex -> IO()
generate (Date y m d) s = putStrLn formattedString
	where	--formattedString = concat (take 20 formattedList)
		formattedString = concat formattedList
		formattedList = map (format initializedDate) listOfPostfixes
		listOfPostfixes = findAllPostfixes firstPostfix
		firstPostfix = findFirstPostfix initializedDate 0
		initializedDate = initialize validDate s
		validDate = isValid (Date y m d)

--yearParser (Date y m d) 	| length (show y) > 2 = (Date (read (drop 2 (show y)) :: Int) m d)
--				| otherwise = (Date y m d)

isValid :: Date -> Date
isValid (Date y m d)	| (y < 0 || y > 99  ) = error "Invalid year"
			| (m < 1 || m > 12) = error "Invalid month"
			| (d < 1 || d > 31 ) = error "Invalid day"
			| otherwise = (Date y m d)