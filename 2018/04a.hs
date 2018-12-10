import System.IO (isEOF)
import Text.Regex.Posix
import Data.List
import qualified Data.Map

data EventLabel = Begin Int | Sleep | Wake deriving Show

data Event = Event { time :: (Int, Int, Int, Int, Int),
                     label :: EventLabel } deriving Show

instance Eq Event where t == s = (time t) == (time s)

instance Ord Event where compare t s = compare (time t) (time s)


data State = Asleep Int Int | Awake Int


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


minute (Event (_, _, _, _, m) label) = m


parseLine :: String -> Event
parseLine line =
    let pattern = "\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\\] (.*)"
        parts = tail $ head (line =~ pattern) :: [String]
        [year, month, day, hour, minute, labelAsString] = parts
        label = parseLabel labelAsString
    in Event (read year, read month, read day, read hour, read minute) label


parseLabel :: String -> EventLabel
parseLabel label = case (head $ words label) of
    "Guard" ->
        let match = head (label =~ "[0-9]+") :: [String]
            [guardId] = match
        in Begin (read guardId)
    "falls" -> Sleep
    "wakes" -> Wake


processEvents :: Maybe State -> [Event] -> Data.Map.Map Int (Data.Map.Map Int Int)

processEvents s [] = Data.Map.empty

processEvents state (event : events) =
    case (label event) of
        Begin g -> processEvents (Just (Awake g)) events
        Sleep   -> let Just (Awake g) = state
                   in processEvents (Just (Asleep g (minute event))) events
        Wake    -> let Just (Asleep g start) = state
                       end = minute event
                       remainder = processEvents (Just (Awake g)) events
                       counters = Data.Map.findWithDefault Data.Map.empty g remainder
                       additions = Data.Map.fromList [(i,1) | i <- [start..(end-1)]]
                       newCounters = Data.Map.unionWith (+) additions counters
                   in Data.Map.insert g newCounters remainder


optimum map =
    let optimumValue = maximum $ Data.Map.elems map
        optimumKeys = Data.Map.keys $ Data.Map.filter (== optimumValue) map
    in minimum optimumKeys


mostSleepyGuard sleepMap =
    let minutesSlept = Data.Map.map (sum . Data.Map.elems) sleepMap
    in optimum minutesSlept
        

mostSleepyMinute sleepyGuard sleepMap =
    let guardSleepMap = sleepMap Data.Map.! sleepyGuard
    in optimum guardSleepMap
        

main = do
    linesAsStrings <- readLines
    let events = sort $ map parseLine linesAsStrings
        sleepMap = processEvents Nothing events
        guard = mostSleepyGuard sleepMap
        minute = mostSleepyMinute guard sleepMap
    print $ guard * minute
