import Prelude 
import System.IO
import Data.Char
import System.Random
import System.Environment

data Ninja = Ninja {name:: String, country:: Char,
                    status:: String, score:: Float,
                    exam1:: Float, exam2:: Float,
                    ability1:: String, ability2:: String,
                    r:: Int}
                    deriving Show

showNinja :: Ninja -> IO()
showNinja x = do
			putStr (name x)
			putStr ", Score: "
			putStr (show (score x))
			putStr ", Status: "
			putStr (status x)
			putStr ", Round: "
			putStr (show (r x))
			putStrLn ""
			
printJourneyMan :: [Ninja] -> IO()
printJourneyMan [] = putStrLn "No journeyman found"
printJourneyMan (x:xs) = do
							if (length (status x)) > 7
								then showNinja x
								else printJourneyMan xs
								
exit :: IO()
exit = do
		printJourneyMan fire
		printJourneyMan lightning
		printJourneyMan water
		printJourneyMan wind
		printJourneyMan earth

fire :: [Ninja]
fire = [] ++ [Ninja "Sasuke" 'F' "Junior" 0 50 60 "Lightning" "Fire" 0]
		++ [Ninja "Neiji" 'F' "Junior" 0 40 75 "Vision" "Hit" 0]
		++ [Ninja "Naruto" 'F' "JorneyMan" 0 40 45 "Clone" "Summon" 3]

lightning :: [Ninja]
lightning = [] ++ [Ninja"Sana" 'L' "Junior" 0 55 65 "Lightning" "Hit" 0] 
		++[Ninja"Aimi" 'L' "Junior" 0 60 65 "Blade" "Rock" 0] 
		++[Ninja"Kira" 'L' "Junior" 0 40 60 "Storm" "Rock" 0]

water :: [Ninja]
water = [] ++ [Ninja"Midare" 'W' "Junior" 0 35 45 "Hit" "Water" 0]
		++ [Ninja"Suiu" 'W' "Junior" 0 45 55 "Water" "Blade" 0]
		++ [Ninja"Samidare" 'W' "Junior" 0 30 55 "Water" "Hit" 0]

wind :: [Ninja]
wind = [] ++ [Ninja"Gaara" 'N' "Junior" 0 55 80 "Vision" "Sand" 0]
			++ [Ninja"Temari" 'N' "Junior" 0 40 60 "Hit" "Blade" 0]
			++ [Ninja"Kankuro" 'N' "Junior" 0 30 50 "Hit" "Storm" 0]

earth :: [Ninja]
earth = [] ++ [Ninja"Haruki" 'E' "Junior" 0 50 60 "Sand" "Fire" 0]
		++ [Ninja"Miyazaki" 'E' "Junior" 0 45 55 "Rock" "Hit" 0]
		++ [Ninja"Hiroshi" 'E' "Junior" 0 40 60 "Storm" "Rock" 0]
		
firstCountryNinja :: Char -> [[Ninja]] -> Ninja
firstCountryNinja a countries
	| a=='F' = ((countries !! 0) !! 0)
	| a=='L' = ((countries !! 1) !! 0)
	| a=='N' = ((countries !! 2) !! 0)
	| a=='E' = ((countries !! 3) !! 0)
	| a=='W' = ((countries !! 4) !! 0)
	
		
roundCountries :: Char -> Char -> [[Ninja]] -> (Ninja, [[Ninja]])
roundCountries a b countries = do
							let n1 = firstCountryNinja a countries
							let n2 = firstCountryNinja b countries
							let winner = roundNinja n1 n2
							if winner==1
								then if (status n1) == "Junior" && (r n1)==2
									then
										((Ninja (name n1) (country n1) "JorneyMan" (score n1) (exam1 n1) (exam2 n1) (ability1 n1) (ability2 n1) 3), updateCountries (Just n1) (Just n2) countries)
									else
										(Ninja (name n1) (country n1) "Junior" (score n1) (exam1 n1) (exam2 n1) (ability1 n1) (ability2 n1) ((r n1)+1), updateCountries (Just n1) (Just n2) countries)
								else  if (status n2) == "Junior" && (r n2)==2
									then
										((Ninja (name n2) (country n2) "JorneyMan" (score n2) (exam1 n2) (exam2 n2) (ability1 n2) (ability2 n2) 3), updateCountries (Just n2) (Just n1) countries)
									else
										(Ninja (name n2) (country n2) "Junior" (score n2) (exam1 n2) (exam2 n2) (ability1 n2) (ability2 n2) ((r n2)+1), updateCountries (Just n2) (Just n1) countries)
									
									
updateCountries :: Maybe Ninja -> Maybe Ninja -> [[Ninja]] -> [[Ninja]]
updateCountries Nothing (Just nRmv) countries@(c:cs) = do
											if (country (c!!0)) == (country nRmv)
												then
													(removeNinja nRmv c) : cs
												else
													c : updateCountries Nothing (Just nRmv) cs
updateCountries (Just nUpd) Nothing countries@(c:cs) = do
											if (country (c!!0)) == (country nUpd)
													then
														(updateNinja nUpd c) : cs
													else
														c : cs
updateCountries (Just nUpd) (Just nRmv) countries@(c:cs) = do
												if (country (c!!0)) == (country nUpd)
													then
														(updateNinja nUpd c) : (updateCountries Nothing (Just nRmv) cs)
													else if (country (c!!0)) == (country nRmv)
														then
															(removeNinja nRmv c) : (updateCountries (Just nUpd) Nothing cs)
														else
															c : updateCountries (Just nUpd) (Just nRmv) cs
															
removeNinja :: Ninja -> [Ninja] -> [Ninja]
removeNinja nRmv [] = error "Ninja to be removed not found!"
removeNinja nRmv ninjas@(n:ns) = do
									if (name nRmv) /= (name n) || (status nRmv) /= (status n) || (score nRmv) /= (score n) ||(exam1 nRmv) /= (exam1 n) || (exam2 nRmv) /= (exam2 n) || (ability1 nRmv) /= (ability1 n) || (ability2 nRmv) /= (ability2 n) || (r nRmv) /= (r n)
										then
											n : removeNinja nRmv ns
										else
											ns
											
updateNinja :: Ninja -> [Ninja] -> [Ninja]
updateNinja nUpd ninjas@(n:ns) = do
									if (name nUpd) /= (name n) || (status nUpd) /= (status n) || (score nUpd) /= (score n) ||(exam1 nUpd) /= (exam1 n) || (exam2 nUpd) /= (exam2 n) || (ability1 nUpd) /= (ability1 n) || (ability2 nUpd) /= (ability2 n) || (r nUpd) /= (r n)
										then
											n : updateNinja nUpd ns
										else if (status nUpd) == "Junior" && (r nUpd)==2
											then
												let toAdd=Ninja (name nUpd) (country nUpd) "JorneyMan" (score nUpd) (exam1 nUpd) (exam2 nUpd) (ability1 nUpd) (ability2 nUpd) 3
												in	toAdd : ns
											else
												let toAdd=Ninja (name nUpd) (country nUpd) "Junior" (score nUpd) (exam1 nUpd) (exam2 nUpd) (ability1 nUpd) (ability2 nUpd) ((r nUpd)+1)
												in toAdd : ns

printWinner :: Ninja -> IO ()
printWinner n = do
					putStr "Winner: "
					showNinja n
								

roundNinja :: Ninja -> Ninja -> Int
roundNinja n1 n2
	|score1 > score2 || (score1==score2 && abilities1 > abilities2) = 1
	|score1 < score2 || (score1==score2 && abilities1 < abilities2) = 2
		where
			abilities1 = (abilityVal (ability1 n1)) + (abilityVal (ability2 n1))
			abilities2 = (abilityVal (ability1 n2)) + (abilityVal (ability2 n2))
			score1=0.5 * (exam1 n1) + 0.3*(exam2 n1) + abilities1
			score2=0.5 * (exam1 n2) + 0.3*(exam2 n2) + abilities2
			--(kot, result) = (randomRange 1 2)
			
randomRange :: Int -> Int -> IO [Int]
randomRange a b = do
				gen <- newStdGen
				return $ (randomRs (a,b::Int) gen)

calcScore :: Ninja -> Float
calcScore n = 0.5 * (exam1 n) + 0.3*(exam2 n) + (abilityVal (ability1 n)) + (abilityVal (ability2 n))

abilityVal :: String -> Float
abilityVal a
	| a=="Hit" || a=="Storm" = 10
	| a=="Clone" || a=="Blade" || a=="Rock" = 20
	| a=="Vision" || a=="Water" = 30
	| a=="Fire" = 40
	| a=="Lightning" || a=="Sand" || a=="Summon" = 50
		
main = do
	putStr "Enter the first country code: "
	hFlush stdout
	--randomRIO (1,2)
	fsCountry <- getChar
	hFlush stdout
	getLine
	putStr "Enter the second country code: "
	hFlush stdout
	secCountry <- getChar
	let tempCountries = [fire, lightning, wind, earth, water]
	let (winner, newCountries) = roundCountries (toUpper fsCountry) (toUpper secCountry) tempCountries
	printWinner winner
	showNinja (firstCountryNinja 'W' tempCountries)
	showNinja (firstCountryNinja 'W' newCountries)
	-- 	The last two functions will print the loser country first ninja (before and after) for country Water.
	--	Change 'W' for other input of loser
	
	
