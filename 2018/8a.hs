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


metaSum (Node meta children) =
    let local = sum meta
        remote = sum $ map (metaSum) children
    in local + remote


main = do
    input <- getLine
    let inputAsNumbers = map (read) (words input) :: [Int]
        (tree, remainder) = parse inputAsNumbers
    print $ metaSum tree
    
