import Data.List
import Data.Ord

--3.	Vytvorte program pre spracovanie v�sledkov volieb. V�sledky volieb nech s�
--	zadan� ako zoznam dvoj�c (n�zov strany, po�et hlasov). Program m� vypo��-
--	ta�, ktor� strany prekonaj� hranicu prechodu do parlamentu a ko�ko z�skaj�
--	poslaneck�ch mand�tov. V�sledok vyp�te v tvare.	
--	Po�et kresiel a hranica pre vstup do parlamentu nech s� zadan� prostredn�ctvom funkci�:
--	Na testovanie programu m��ete pou�i� v�sledky volieb 2010.
--	Postup v�po�tu po�tu poslancov nech je nasledovn� (ide o mierne zjednodu�en� verziu skuto�n�ho postupu):
--	Najprv sa vyber� strany, ktor� prekonali stanoven� hranicu (podiel z celkov�ho po�tu hlasov), a teda sa dostan�
--	do parlamentu. N�sledne sa vypo��ta s��et hlasov za tieto strany a vydel�
--	sa celkov�m po�tom poslancov. Takto z�skan� ��slo nazvime volebn�m ��slom. 
--	Po�et hlasov za ka�d� postupuj�cu stranu sa vydel� volebn�m ��slom a
--	v�sledok celo��seln�ho delenia je predbe�n�m po�tom mand�tov, ktor� strana
--	dostane. Ak tak�mto sp�sobom nebud� pridelen� v�etky mand�ty, zvy�n�
--	mand�ty je potrebn� prideli� (po jednom) stran�m s najv���m zostatkom
--	pri delen� volebn�m ��slom.


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