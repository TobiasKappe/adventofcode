import Data.Char


reacts x y =
    (isLower x && isUpper y && toUpper x == y) ||
    (isUpper x && isLower y && toLower x == y)


reduce [] stack = stack

reduce (letter : letters) [] =
    reduce letters [letter]

reduce (letter : letters) (top : stack) =
    if reacts letter top then reduce letters stack
    else reduce letters (letter : top : stack)


main = do
    input <- getLine
    print $ length $ reduce input []
