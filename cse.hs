import System.Environment
import System.IO
import Data.Char

-- ######### We should add error check when file not found if not implemented already ###########

data Ninja = Ninja {name :: String, country :: Char,
                    status :: String, exam1 :: Float,
                    exam2 :: Float, ability1 :: Float,
                    ability2 :: Float, r :: Int, score :: Float} deriving (Show)
                   

-- For checking if the correct number of command line arguments are given.
checkCommandLineArgs :: Int -> IO ()
checkCommandLineArgs 1 = return ()
checkCommandLineArgs _ = error "Usage: ./<executable> <filename>.txt"

-- For parsing the country data.
parseCountry :: String -> Char
parseCountry country
    | country == "Fire"      = 'F'
    | country == "Lightning" = 'L'
    | country == "Water"     = 'W'
    | country == "Wind"      = 'N'
    | country == "Earth"     = 'E'
    | otherwise              = error "Invalid country."

-- TODO: Create a 'class' or `dictionary` for abilities.
-- For retrieving corresponding damage values of abilities.
getDamage :: String -> Float
getDamage ability
    | ability == "Clone"     = 20
    | ability == "Hit"       = 10
    | ability == "Lightning" = 50
    | ability == "Vision"    = 30
    | ability == "Sand"      = 50
    | ability == "Fire"      = 40
    | ability == "Water"     = 30
    | ability == "Blade"     = 20
    | ability == "Summon"    = 50
    | ability == "Storm"     = 10
    | ability == "Rock"      = 20
    | otherwise              = error "Invalid ability."

-- For calculating the score of a ninja.
getScore :: Float -> Float -> Float -> Float -> Float
getScore exam1 exam2 ability1 ability2 = 0.5*exam1 + 0.3*exam2 + ability1 + ability2

-- For reading the ninja information and returning a ninja.
readNinja :: [String] -> Ninja
readNinja [name, country, exam1, exam2, ability1, ability2] =
    Ninja name c "Junior" exam1Result exam2Result damage1 damage2 0 score
        where
            c           = parseCountry country
            exam1Result = read exam1 :: Float
            exam2Result = read exam2 :: Float
            damage1     = getDamage ability1
            damage2     = getDamage ability2
            score       = getScore exam1Result exam2Result damage1 damage2


-- For reading all the information about the ninjas and returning a list of ninjas.
readNinjas :: [String] -> [Ninja]
readNinjas []           = []
readNinjas (stat:stats) = (readNinja $ words stat):(readNinjas stats)

getNinjas :: [Ninja] -> Char -> [Ninja]
getNinjas ninjas c = filter (\ninja -> (country ninja == c)) ninjas

getFire :: [Ninja] -> [Ninja]
getFire ninjas = getNinjas ninjas 'F'

getLigthning :: [Ninja] -> [Ninja]
getLigthning ninjas = getNinjas ninjas 'L'

getWater :: [Ninja] -> [Ninja]
getWater ninjas = getNinjas ninjas 'W'

getWind :: [Ninja] -> [Ninja]
getWind ninjas = getNinjas ninjas 'N'

getEarth :: [Ninja] -> [Ninja]
getEarth ninjas = getNinjas ninjas 'E'

separate :: [Ninja] -> [[Ninja]]
separate ninjas = [getFire ninjas, getLigthning ninjas, getWater ninjas, getWind ninjas, getEarth ninjas]


printMenu :: IO ()
printMenu = do
	hSetBuffering stdout NoBuffering
	putStrLn "a) View a Count's Ninja Information"
	putStrLn "b) View All Countries' Ninja Information"
	putStrLn "c) Make a Round Between Ninjas"
	putStrLn "d) Make a Round Between Countries"
	putStrLn "e) Exit"
	

playGame :: [[Ninja]] -> IO ()
playGame ninjas = do
	hSetBuffering stdout NoBuffering
	printMenu
	putStr "Enter the action: "
	--hFlush stdout
	user_input <- getLine

	case user_input of
		"a" -> do
			playGame ninjas
		"b" -> do
			playGame ninjas
		"c" -> do
			putStr "Enter the name of the first ninja: "
			fsNinja <- getLine
			putStr "Enter the country code of the first ninja: "
			fsCountry <- getChar
			getLine
			let n1 = selNinja fsNinja (toUpper fsCountry) ninjas
			putStr "Enter the name of the second ninja: "
			secNinja <- getLine
			putStr "Enter the country code of the second ninja: "
			secCountry <- getChar
			getLine
			let n2 = selNinja secNinja (toUpper secCountry) ninjas
			let (winner, newNinjas) = roundNinja n1 n2 ninjas
			printWinner winner
			playGame newNinjas
		"d" -> do
			putStr "Enter the first country code: "
			fsCountry <- getChar
			getLine
			putStr "Enter the second country code: "
			secCountry <- getChar
			getLine
			let (winner, newNinjas) = roundCountries (toUpper fsCountry) (toUpper secCountry) ninjas
			printWinner winner
			playGame newNinjas
		"e" -> do
			return ()
		_ 	-> do
			putStrLn "Please enter a correct action"
			playGame ninjas

printWinner :: Ninja -> IO ()
printWinner n = do
	hSetBuffering stdout NoBuffering
	putStr "Winner: "
	showNinja n
	
showNinja :: Ninja -> IO()
showNinja x = do
	hSetBuffering stdout NoBuffering
	putStr (name x)
	putStr ", Score: "
	putStr (show (score x))
	putStr ", Status: "
	putStr (status x)
	putStr ", Round: "
	putStr (show (r x))
	putStrLn ""
	--hFlush stdout
	
roundNinja :: Ninja -> Ninja -> [[Ninja]] -> (Ninja, [[Ninja]])
roundNinja n1 n2 countries = do
	let winner = duel n1 n2
	if winner==1
		then if (status n1) == "Junior" && (r n1)==2
			then
				((Ninja (name n1) (country n1) "JourneyMan" (exam1 n1) (exam2 n1) (ability1 n1) (ability2 n1) 3 ((score n1)+10)), updateCountries (Just n1) (Just n2) countries)
			else
				((Ninja (name n1) (country n1) "Junior" (exam1 n1) (exam2 n1) (ability1 n1) (ability2 n1) ((r n1)+1) ((score n1)+10)), updateCountries (Just n1) (Just n2) countries)
		else  if (status n2) == "Junior" && (r n2)==2
			then
				((Ninja (name n2) (country n2) "JourneyMan" (exam1 n2) (exam2 n2) (ability1 n2) (ability2 n2) 3 ((score n2)+10)), updateCountries (Just n2) (Just n1) countries)
			else
				((Ninja (name n2) (country n2) "Junior" (exam1 n2) (exam2 n2) (ability1 n2) (ability2 n2) ((r n2)+1) ((score n2)+10)), updateCountries (Just n2) (Just n1) countries)

roundCountries :: Char -> Char -> [[Ninja]] -> (Ninja, [[Ninja]])
roundCountries a b countries = do
	let n1 = firstCountryNinja a countries
	let n2 = firstCountryNinja b countries
	roundNinja n1 n2 countries

firstCountryNinja :: Char -> [[Ninja]] -> Ninja
firstCountryNinja a countries
	| a=='F' = ((countries !! 0) !! 0)
	| a=='L' = ((countries !! 1) !! 0)
	| a=='W' = ((countries !! 2) !! 0)
	| a=='N' = ((countries !! 3) !! 0)
	| a=='E' = ((countries !! 4) !! 0)
	
duel :: Ninja -> Ninja -> Int
duel n1 n2
	|score1 > score2 || (score1==score2 && abilities1 > abilities2) = 1
	|score1 < score2 || (score1==score2 && abilities1 < abilities2) = 2
	--Random function needs to be inserted as a guard here
		where
			abilities1 = (ability1 n1) + (ability2 n1)
			abilities2 = (ability1 n2) + (ability2 n2)
			score1=score n1
			score2=score n2
			--(kot, result) = (randomRange 1 2)

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
				c : updateCountries (Just nUpd) Nothing cs
updateCountries (Just nUpd) (Just nRmv) countries@(c:cs) = do
	if (country nUpd) == (country nRmv)
		then if (country nUpd) == (country (c!!0))
			then (updateNRemove nUpd nRmv c) : cs
			else c : (updateCountries (Just nUpd) (Just nRmv) cs)
		else if (country (c!!0)) == (country nUpd)
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
				let toAdd=Ninja (name nUpd) (country nUpd) "JourneyMan" (exam1 nUpd) (exam2 nUpd) (ability1 nUpd) (ability2 nUpd) 3 ((score nUpd)+10)
				in	toAdd : ns
			else
				let toAdd=Ninja (name nUpd) (country nUpd) (status nUpd) (exam1 nUpd) (exam2 nUpd) (ability1 nUpd) (ability2 nUpd) ((r nUpd)+1) ((score nUpd)+10)
				in toAdd : ns

updateNRemove :: Ninja -> Ninja -> [Ninja] -> [Ninja]
updateNRemove nUpd nRmv ninjas@(n:ns) = do
	if (name nUpd) /= (name n) || (status nUpd) /= (status n) || (score nUpd) /= (score n) ||(exam1 nUpd) /= (exam1 n) || (exam2 nUpd) /= (exam2 n) || (ability1 nUpd) /= (ability1 n) || (ability2 nUpd) /= (ability2 n) || (r nUpd) /= (r n)
		then if (name nRmv) /= (name n) || (status nRmv) /= (status n) || (score nRmv) /= (score n) ||(exam1 nRmv) /= (exam1 n) || (exam2 nRmv) /= (exam2 n) || (ability1 nRmv) /= (ability1 n) || (ability2 nRmv) /= (ability2 n) || (r nRmv) /= (r n)
				then
					n : updateNRemove nUpd nRmv ns
				else
					updateNinja nUpd ns
		else if (status nUpd) == "Junior" && (r nUpd)==2
			then
				let toAdd=Ninja (name nUpd) (country nUpd) "JourneyMan" (exam1 nUpd) (exam2 nUpd) (ability1 nUpd) (ability2 nUpd) 3 ((score nUpd)+10)
				in	toAdd : removeNinja nRmv ns
			else
				let toAdd=Ninja (name nUpd) (country nUpd) (status nUpd) (exam1 nUpd) (exam2 nUpd) (ability1 nUpd) (ability2 nUpd) ((r nUpd)+1) ((score nUpd)+10)
				in toAdd : removeNinja nRmv ns
								
selNinja :: String -> Char -> [[Ninja]] -> Ninja
selNinja a ccode countries
	| ccode=='F' = findNinja a (countries !! 0)
	| ccode=='L' = findNinja a (countries !! 1)
	| ccode=='W' = findNinja a (countries !! 2)
	| ccode=='N' = findNinja a (countries !! 3)
	| ccode=='E' = findNinja a (countries !! 4)
	
findNinja :: String -> [Ninja] -> Ninja
findNinja a [] = error "Ninja could not be found"
findNinja a ninjas@(n:ns)= do
	if((name n) == a)
		then n
		else findNinja a ns
					
main :: IO ()
main = do
    -- Get command line arguments.
    args <- getArgs
    checkCommandLineArgs (length args)

    -- Get file name from the command line arguments.
    let fileName = args !! 0
    content <- readFile fileName

    -- Create a list of ninjas.
    let stats = lines content
    let ninjas = readNinjas stats

    -- Create nations.
    let [fire, lightning, water, wind, earth] = separate ninjas


    playGame [fire, lightning, water, wind, earth]
