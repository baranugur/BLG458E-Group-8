import System.Environment
import System.IO
import Data.Char

-- TODO: Create a 'class' for abilities.
-- TODO: Calculate scores.

data Ninja = Ninja {name :: String, country :: Char,
                    status :: String, exam1 :: Float,
                    exam2 :: Float, ability1 :: String,
                    ability2 :: String, r :: Int} deriving (Show)

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

-- For reading the ninja information and returning a ninja.
readNinja :: [String] -> Ninja
readNinja [name, country, exam1, exam2, ability1, ability2] = Ninja name (parseCountry country) "junior" (read exam1 :: Float) (read exam2 :: Float) ability1 ability2 0

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
    print wind
