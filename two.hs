{- http://www.haskell.org/haskellwiki/99_questions/11_to_20 -}


-- Problem 11
-- (Carried from Problem 9)
pack :: Eq a => [a] -> [[a]]
pack []  = [[]]
pack [x] = [[x]]
pack (x:xs)
    | (x == head xs) = (x:(head rest)):tail rest
    | otherwise      = [x]:rest
    where rest = pack xs

data Result c = Single c | Multiple Int c deriving Show
count_sublist :: [a] -> Result a
count_sublist xs
    | total > 1 = Multiple total (head xs)
    | otherwise = Single (head xs)
    where total = (sum [1 | _ <- xs])

encodeModified :: Eq a => [a] -> [Result a]
encodeModified = map count_sublist . pack

-- Problem 12
decodeModified :: [Result a] -> [a]
decodeModified []                  = []
decodeModified (Single c:xs)       = c:decodeModified xs
decodeModified (Multiple 1 c:xs)   = c:decodeModified xs
decodeModified (Multiple n c:xs)   = c:decodeModified (Multiple (n-1) c:xs)

-- Problem 14
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x:dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) k = [x | _ <- [1,2..k]] ++ repli xs k

-- Problem 16
dropEveryHelper :: [a] -> Int -> Int -> [a]
dropEveryHelper [] _ _ = []
dropEveryHelper (x:xs) k i
    | i `mod` k == 0 = dropEveryHelper xs k (i+1)
    | otherwise      = x:dropEveryHelper xs k (i+1)

dropEvery :: [a] -> Int -> [a]
dropEvery lst k = dropEveryHelper lst k 1

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split lst 0 = ([], lst)
split (x:xs) k = let rest = split xs (k-1) in
            (x:(fst rest), snd rest)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice (x:xs) 1 1 = [x]
slice (x:xs) 1 i = x:slice xs 1 (i-1)
slice (x:xs) i j = slice xs (i-1) (j-1)

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt k (x:xs) =  let rest = removeAt (k-1) xs in
            (fst rest, x:(snd rest))

