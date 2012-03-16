--3.kat -- 4uloha

data Date = Date Int Int Int deriving (Show)
data Sex = Male | Female deriving (Eq)

initialize :: Date -> Sex -> Date
initialize (Date y m d) s	| (s == Female) = (Date y (m+50) d)
			 	| otherwise = (Date y m d)

findFirst :: Date -> Int -> Int
findFirst (Date y m d) postfix 	| ((y+m+d+postfix) `mod` 11 == 0) = postfix
				| otherwise = findFirst (Date y m d) (succ postfix)

findAll :: Int -> [Int]
findAll postfix = [postfix + (11 * i) | i <- [0..], postfix + (11 * i) < 10000]

format :: Date -> Int -> String
format (Date y m d) postfix = show y ++ (format' m 2) ++ (format' d 2) ++ "/" ++ (format' postfix 4) ++ "\n"

format' :: Int -> Int -> String
format' number expectedDigits = zeros ++ show number
	where	zeros = [ '0' | _ <- [1..n]]
		n = expectedDigits - length (show number)

generate :: Date -> Sex -> IO()
generate (Date y m d) s = putStrLn (concat (take 20 (map (format (initialize (Date y m d) s)) (findAll (findFirst (initialize (Date y m d) s) 0)))))
--generate (Date y m d) s = putStrLn (concat (map (format (initialize (Date y m d) s)) (findAll (findFirst (Date y m d) 0))))

-- interval toho cyklu a skraslit kod