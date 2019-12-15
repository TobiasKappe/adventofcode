import System.IO (isEOF)
import Text.Regex.Posix
import Data.List.Extra as L


data Bot = Bot { x :: Int,
                 y :: Int,
                 z :: Int,
                 r :: Int } deriving Show


manhattan bot1 bot2 =
    sum [abs ((x bot1) - (x bot2)),
         abs ((y bot1) - (y bot2)),
         abs ((z bot1) - (z bot2))]


inRange bot1 bot2 =
    manhattan bot1 bot2 <= r bot1


parseBot :: String -> Bot

parseBot line =
    let pattern = "pos=<(.*),(.*),(.*)>, r=(.*)"
        parts = tail $ head (line =~ pattern)
        [x, y, z, radius] = map (read) parts
    in Bot x y z radius


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


main = do
    lines <- readLines
    let bots = map (parseBot) lines
        strongest = L.maximumOn (r) bots
        reachable = L.filter (inRange strongest) bots
    print $ length reachable
