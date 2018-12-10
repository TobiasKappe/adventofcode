data Tree = Node [Int] [Tree] deriving Show


process n func state =
    case n of
        0 -> ([], state)
        n -> let (x, newState) = func state
                 (xs, finalState) = process (n-1) func newState
             in (x : xs, finalState)


prod f g x = (f x, g x)


parse (childcount : metacount : afterHeader) =
    let (children, afterChildren) = process childcount (parse) afterHeader
        (meta, afterMeta) = process metacount (prod (head) (tail)) afterChildren
    in (Node meta children, afterMeta)



crossRef [] values = []

crossRef (index : indices) values =
    let local = if index < length values
                then values !! index
                else 0
    in local : crossRef indices values


value (Node meta []) =
    sum meta


value (Node meta children) =
    let properMeta = map ((+) (-1)) meta
        childValues = crossRef properMeta (map (value) children)
    in sum childValues


main = do
    input <- getLine
    let inputAsNumbers = map (read) (words input) :: [Int]
        (tree, remainder) = parse inputAsNumbers
    print $ value tree
    
