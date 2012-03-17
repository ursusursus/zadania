--3.kat -- 4uloha

data Date = Date Int Int Int deriving (Show)
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
format (Date y m d) postfix = show y ++ (format' m 2) ++ (format' d 2) ++ "/" ++ (format' postfix 4) ++ "\n"

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
		initializedDate = initialize (Date y m d) s
