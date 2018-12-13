import qualified Data.Map.Strict as M
import qualified Data.List as L
import System.IO (isEOF)


data Direction = East | West | North | South deriving (Eq, Show)

data Turn = Left | Right | Straight deriving (Eq, Show)

data Cart = Cart (Int, Int) Direction Turn deriving (Eq, Show)

instance Ord Cart where
    compare (Cart (x1, y1) _ _) (Cart (x2, y2) _ _) = 
        compare (y1, x1) (y2, x2)

type Tracks = M.Map (Int, Int) Direction

data Gadget = Slash | Backslash | Junction deriving (Eq, Show)

type Object = Either Direction Gadget


updatePosition (x, y) direction =
    case direction of
        East -> (x+1, y)
        West -> (x-1, y)
        South -> (x, y+1)
        North -> (x, y-1)


updateDirection gadget direction turn =
    case gadget of
        Backslash -> case direction of
            East -> South
            North -> West
            West -> North
            South -> East
        Slash -> case direction of
            East -> North
            North -> East
            West -> South
            South -> West
        Junction -> case direction of
            North -> case turn of
                Main.Straight -> North
                Main.Left -> West
                Main.Right -> East
            South -> case turn of
                Main.Straight -> South
                Main.Left -> East
                Main.Right -> West
            West -> case turn of
                Main.Straight -> West
                Main.Left -> South
                Main.Right -> North
            East -> case turn of
                Main.Straight -> East
                Main.Left -> North
                Main.Right -> South


updateTurn turn =
    case turn of
        Main.Left -> Main.Straight
        Main.Straight -> Main.Right
        Main.Right -> Main.Left


updateCart tracks (Cart position direction turn) =
    let newPosition = updatePosition position direction
        newDirection = if M.member newPosition tracks
            then let Just gadget = M.lookup newPosition tracks
                 in updateDirection gadget direction turn
            else direction
        newTurn = if M.lookup newPosition tracks == Just Junction
            then updateTurn turn
            else turn
    in Cart newPosition newDirection newTurn


collides (Cart p1 _ _) (Cart p2 _ _) =
    p1 == p2


simulate tracks carts [] =
    simulate tracks [] (L.sort carts)

simulate tracks carts (cart : queue) =
    let newCart = updateCart tracks cart
    in if any (collides newCart) (carts ++ queue)
       then let Cart position _ _ = newCart
            in position
       else simulate tracks (newCart : carts) queue


readLines = do
    done <- isEOF
    if done
    then return []
    else do
        line <- getLine
        rest <- readLines
        return (line : rest)


parseChar char =
    case char of
        '^'  -> Just (Prelude.Left North)
        '>'  -> Just (Prelude.Left East)
        'v'  -> Just (Prelude.Left South)
        '<'  -> Just (Prelude.Left West)
        '+'  -> Just (Prelude.Right Junction)
        '/'  -> Just (Prelude.Right Slash)
        '\\' -> Just (Prelude.Right Backslash)
        _    -> Nothing


parseLine row column [] =
    ([], M.empty)

parseLine row column (char : chars) =
    let (carts, tracks) = parseLine row (column+1) chars
    in case parseChar char of
        Just (Prelude.Left direction) -> 
            let newCart = Cart (column, row) direction Main.Left
            in (newCart : carts, tracks)
        Just (Prelude.Right gadget) ->
            let newTracks = M.insert (column, row) gadget tracks
            in (carts, newTracks)
        Nothing ->
            (carts, tracks)


parseLines row  [] =
    ([], M.empty)

parseLines row (line : lines) =
    let (newCarts, newTracks) = parseLine row 0 line
        (otherCarts, otherTracks) = parseLines (row+1) lines
        finalCarts = newCarts ++ otherCarts
        finalTracks = M.union newTracks otherTracks
    in (finalCarts, finalTracks)


main = do
    lines <- readLines
    let (carts, tracks) = parseLines 0 lines
        crash = simulate tracks carts []
    print crash
