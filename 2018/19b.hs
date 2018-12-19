testFactor input candidate =
    if input `mod` candidate == 0
    then candidate
    else 0


main = do
    let input = 10551417
        factors = map (testFactor input) [1..input]
        result = sum factors
    print $ result
