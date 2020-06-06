import System.Environment
import System.IO
import Data.Char

data Ninja = Ninja {name:: String, country:: Char,
                    status:: String, exam1:: Float,
                    exam2:: Float, ability1:: String,
                    ability2:: String, r:: Int} deriving (Show)

fire :: [Ninja]
fire = []

lightning :: [Ninja]
lightning = []

water :: [Ninja]
water = []

wind :: [Ninja]
wind = []

earth :: [Ninja]
earth = []

-- For checking if the correct number of command line arguments are given.
checkCommandLineArgs :: Int -> IO ()
checkCommandLineArgs 1 = return ()
checkCommandLineArgs _ = error "Usage: ./<executable> <filename>.txt"

-- For reading the ninja information and returning a ninja.
readNinja :: [String] -> Ninja
readNinja [name, country, exam1, exam2, ability1, ability2] = Ninja name (toUpper $ head country) "junior" (read exam1 :: Float) (read exam2 :: Float) ability1 ability2 0

-- For reading all the information about the ninjas and returning a list of ninjas.
readNinjas :: [String] -> [Ninja]
readNinjas []           = []
readNinjas (stat:stats) = (readNinja $ words stat):(readNinjas stats)

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
    print ninjas
