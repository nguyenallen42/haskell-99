{- http://www.haskell.org/haskellwiki/99_questions/1_to_10 -}

-- Problem 1
myLast :: [a] -> a
myLast l = last l

myLast' :: [a] -> a
myLast' [a]    = a
myLast' (x:xs) = myLast' xs

-- Problem 2
myButLast :: [a] -> a
myButLast l = last (init l)

myButLast' :: [a] -> a
myButLast' [a,b]  = a
myButLast' (x:xs) = myButLast' xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt l 0 = head l
elementAt l k = elementAt (tail l) (k - 1)

-- Problem 4
myLength :: [a] -> Int
myLength l = sum [1 | _ <- l]

myLength' :: [a] -> Int
myLength' []     = 0
myLength' (x:xs) = 1 + myLength' xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse(xs) ++ [x]

swap :: [a] -> a -> [a]
swap x y = y:x

myReverse' :: [a] -> [a]
myReverse' l = foldl swap [] l

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome l   = (head l == last l) && isPalindrome (tail (init l))

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' l = l == reverse l

-- Problem 7
data NestedList a = Elem a | List [NestedList a] deriving Show
flatten :: NestedList a -> [a]
flatten (Elem a)      = [a]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress []  = []
compress [x] = [x]
compress (x:xs)
    | (x == head xs) = compress xs
    | otherwise      = x:compress xs

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack []  = [[]]
pack [x] = [[x]]
pack (x:xs)
    | (x == head xs) = (x:(head rest)):tail rest
    | otherwise      = [x]:rest
    where rest = pack xs

-- Problem 10
count_sublist :: [a] -> (Int, a)
count_sublist xs = (sum [1 | _ <- xs], head xs)

encode :: Eq a => [a] -> [(Int, a)]
encode = map count_sublist . pack

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x
