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
myLength' [] = 0
myLength' (x:xs) = 1 + myLength' xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse(xs) ++ [x]

swap :: [a] -> a -> [a]
swap x y = y:x

myReverse' :: [a] -> [a]
myReverse' l = foldl swap [] l

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []     = True
isPalindrome [x]    = True
isPalindrome [x, y] = False
isPalindrome l      = (head l == last l) && isPalindrome (tail (init l))

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' l = l == reverse l

factorial :: Int -> Int
factorial n = product [1..n]
