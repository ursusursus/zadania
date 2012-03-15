import Data.List
import Data.Ord

--4.kat: 3 uloha--

numberOfSeats :: Int
numberOfSeats = 150

threshold :: Float
threshold = 0.05

elections :: [(String, Int)]
elections = [("Smer-SD", 880111), ("SDKU-DS", 390042), ("SaS", 307287), ("KDH", 215755), ("Most-Hid", 205538), ("SNS", 128490), ("SMK", 109638), ("LS-HZDS", 109480), ("SDL", 61137), ("LSNS", 33724), ("KSS", 21104), ("Unia", 17741), ("Paliho Kapurkova", 14576), ("EDS", 10332), ("ND", 7962), ("SRK", 6947), ("ZRS", 6196), ("AZEN", 3325)]

format :: (String, Int) ->  String
format (x,y) = x ++ ".........." ++ show y ++ "\n"

--output = putStrLn (concat (map format elections))

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

--electionNumber = totalVotesInParliament / numberOfSeats
electionNumber = totalVotesInParliament `div` numberOfSeats

remainders = [ (x, y `mod` electionNumber) | (x,y) <- inParliament]
tempMandates = [ (x, y `div` electionNumber) | (x,y) <- inParliament]
totalMandates = total tempMandates

needed = numberOfSeats - totalMandates
magic = reverse (sortBy (comparing snd) remainders)

foo = take needed magic

--SO CLOSE!!!!--
mandates = [(x,y + remainder x) | (x,y) <- tempMandates] 
	where remainder x = if(x`elem` map (fst) foo) then 1 else 0

output1 = putStrLn (concat (map format tempMandates))
output = putStrLn (concat (map format mandates))