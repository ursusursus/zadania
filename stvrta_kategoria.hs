import Data.List
import Data.Ord

--3.	Vytvorte program pre spracovanie výsledkov volieb. Výsledky volieb nech sú
--	zadané ako zoznam dvojíc (názov strany, poèet hlasov). Program má vypoèí-
--	ta, ktoré strany prekonajú hranicu prechodu do parlamentu a ko¾ko získajú
--	poslaneckých mandátov. Výsledok vypíšte v tvare.	
--	Poèet kresiel a hranica pre vstup do parlamentu nech sú zadané prostredníctvom funkcií:
--	Na testovanie programu môžete použi výsledky volieb 2010.
--	Postup výpoètu poètu poslancov nech je nasledovný (ide o mierne zjednodušenú verziu skutoèného postupu):
--	Najprv sa vyberú strany, ktoré prekonali stanovenú hranicu (podiel z celkového poètu hlasov), a teda sa dostanú
--	do parlamentu. Následne sa vypoèíta súèet hlasov za tieto strany a vydelí
--	sa celkovým poètom poslancov. Takto získané èíslo nazvime volebným èíslom. 
--	Poèet hlasov za každú postupujúcu stranu sa vydelí volebným èíslom a
--	výsledok celoèíselného delenia je predbežným poètom mandátov, ktoré strana
--	dostane. Ak takýmto spôsobom nebudú pridelené všetky mandáty, zvyšné
--	mandáty je potrebné prideli (po jednom) stranám s najväèším zostatkom
--	pri delení volebným èíslom.


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