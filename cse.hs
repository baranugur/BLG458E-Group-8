import System.Environment
import System.IO
import Data.Char

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
    Ninja name c "junior" exam1Result exam2Result damage1 damage2 0 score
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
    putStrLn "a) View a Count's Ninja Information"
    putStrLn "b) View All Countries' Ninja Information"
    putStrLn "c) Make a Round Between Ninjas"
    putStrLn "d) Make a Round Between Countries"
    putStrLn "e) Exit"

playGame :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> IO ()
playGame fire lightning water wind earth = do
    printMenu
    putStr "Enter the action: "
    hFlush stdout
    user_input <- getLine
    if user_input == "a"
        then playGame fire lightning water wind earth
        else if user_input == "b"
            then playGame fire lightning water wind earth
            else if user_input == "c"
                then playGame fire lightning water wind earth
                else if user_input == "d"
                    then playGame fire lightning water wind earth
                    else if user_input == "e"
                        then return ()
                        else do
                            putStrLn "Wrong input."
                            playGame fire lightning water wind earth


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
    playGame fire lightning water wind earth
