import Data.Char
import qualified Data.Set


reacts x y =
    (isLower x && isUpper y && toUpper x == y) ||
    (isUpper x && isLower y && toLower x == y)


reduce [] stack = stack

reduce (letter : letters) [] =
    reduce letters [letter]

reduce (letter : letters) (top : stack) =
    if reacts letter top then reduce letters stack
    else reduce letters (letter : top : stack)


unique = Data.Set.elems . Data.Set.fromList

lengthAfterRemoving letters polymer =
    let stripped = filter (\s -> polymer /= toLower s) letters
    in length $ reduce stripped []


main = do
    input <- getLine
    let unpolarised = map toLower input
        polymers = unique unpolarised
        lengths = map (lengthAfterRemoving input) polymers
        shortest = minimum lengths
    print shortest
