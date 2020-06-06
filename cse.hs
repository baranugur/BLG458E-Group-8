import Prelude 
import System.IO
import Data.Char
import System.Random

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
		++ [Ninja "Naruto" 'F' "JuniorMan" 0 40 45 "Clone" "Summon" 3]

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
	
		
roundCountries :: Char -> Char -> [[Ninja]] -> IO ()
roundCountries a b countries = do
							let n1 = firstCountryNinja a countries
							let n2 = firstCountryNinja b countries
							let winner = roundNinja n1 n2
							putStr "Winner: "
							if winner==1
								then
									showNinja n1
								else
									showNinja n2
								

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
	roundCountries (toUpper fsCountry) (toUpper secCountry) tempCountries
	-- 	The round countries will return the updated array to another variable
	--	which will get return from the whole d) operation again to the recurseive initial menu function
	
	
