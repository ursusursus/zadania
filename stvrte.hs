import Data.List
import Data.Ord

--4.kat: 3 uloha--

numberOfSeats :: Int
numberOfSeats = 150

threshold :: Float
threshold = 0.05

elections :: [(String, Int)]
elections = [("Smer-SD", 880111), ("SDKU-DS", 390042), ("SaS", 307287), ("KDH", 215755), ("Most-Hid", 205538), ("SNS", 128490), ("SMK", 109638), ("LS-HZDS", 109480), ("SDL", 61137), ("LSNS", 33724), ("KSS", 21104), ("Unia", 17741), ("Paliho Kapurkova", 14576), ("EDS", 10332), ("ND", 7962), ("SRK", 6947), ("ZRS", 6196), ("AZEN", 3325)]

total :: [(String, Int)] -> Int
total xs = sum (map snd xs)

totalVotes :: Int
totalVotes  = total elections

minimumVotes :: Float
minimumVotes = fromIntegral(totalVotes) * threshold

hasReachedThreshold :: Int -> Bool
hasReachedThreshold x = (fromIntegral(x) > minimumVotes)

inParliament = [(x,y) | (x,y) <- elections, hasReachedThreshold y]

totalVotesInParliament :: Int
totalVotesInParliament = total inParliament

electionNumber :: Int
--electionNumber = totalVotesInParliament / numberOfSeats
electionNumber = totalVotesInParliament `div` numberOfSeats

tempMandates :: [(String,Int)]
tempMandates = [ (x, y `div` electionNumber) | (x,y) <- inParliament]

remainders :: [(String,Int)]
remainders = [ (x, y `mod` electionNumber) | (x,y) <- inParliament]

totalTempMandates :: Int
totalTempMandates = total tempMandates

mandates :: [(String, Int)]
mandates	| ((numberOfSeats - totalTempMandates) == 0) = tempMandates
		| otherwise = [(x,y + additionalMandate x) | (x,y) <- tempMandates] 
			where	additionalMandate x = if(x`elem` map (fst) additionallyIncremented) then 1 else 0
				additionallyIncremented = take (numberOfSeats - totalTempMandates) sortedByRemainder
				sortedByRemainder = reverse (sortBy (comparing snd) remainders)


outputSize :: Int
outputSize = 30


format :: (String, Int) ->  String
format (x,y) = x ++ dots ++ show y ++ "\n"
	where dots = ['.'| _ <- [1..n]]
		where n = outputSize - length x - length (show y) 

output1 = putStrLn (concat (map format tempMandates))
output = putStrLn (concat (map format mandates))