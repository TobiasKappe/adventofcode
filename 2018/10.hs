import Data.List
import System.IO (isEOF)
import Text.Regex.Posix
import qualified Data.Set as S


data Point = Point { x :: Int, y :: Int, vx :: Int, vy :: Int } 
    deriving Show


instance Eq Point where
    p1 == p2 = (x p1 == x p2) && (y p1 == y p2)

instance Ord Point where 
    compare p1 p2 = compare [y p1, x p1] [y p2, x p2]


step ticks p =
    let newx = (x p)+(vx p)*ticks
        newy = (y p)+(vy p)*ticks
    in Point newx newy (vx p) (vy p)


gridStep ticks grid =
    sort $ map (step ticks) grid


firstJump grid =
    let p1 = head $ filter ((> 0) . vy) grid
        p2 = head $ filter ((< 0) . vy) grid
        steps = ((y p2) - (y p1)) `div` ((vy p1) - (vy p2))
        fudge = 5
    in maximum [0, (abs steps) - fudge]


renderLine points [] = putChar '\n'

renderLine [] (cell : cells) =
    do putChar '.'
       renderLine [] cells

renderLine (point : points) (cell : cells) =
    if snd cell /= y point
    then renderLine points (cell : cells)
    else if fst cell == x point
         then do putChar '#'
                 renderLine (dropWhile (== point) points) cells
         else do putChar '.'
                 renderLine (point : points) cells

render points =
    let west = (minimum $ map (x) points) - 2
        east = (maximum $ map (x) points) + 2
        north = (minimum $ map (y) points) - 1
        south = (maximum $ map (y) points) + 1
        cells = [[(i, j) | i <- [west..east]] | j <- [north..south]]
    in mapM (renderLine points) cells



height grid =
    let north = minimum $ map (y) grid
        south = maximum $ map (y) grid
    in south - north


search grid =
    let nextGrid = gridStep 1 grid
        thisHeight = height grid
        nextHeight = height nextGrid
    in if nextHeight > thisHeight
       then (0, grid)
       else let (steps, finalGrid) = search nextGrid
            in (steps+1, finalGrid)


parseLine :: String -> Point

parseLine line =
    let pattern = "position=<(.*),(.*)> velocity=<(.*),(.*)>"
        parts = tail $ head (line =~ pattern) :: [String]
        [px, py, vx, vy] = parts
    in Point (read px) (read py) (read vx) (read vy)


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
    let points = sort $ map (parseLine) lines
        jump = firstJump points
        afterJump = gridStep jump points
        (steps, afterSearch) = search afterJump
    print (jump + steps)
    render $ afterSearch
