-- Basic Concepts

import Data.Char

fac :: Int -> Int
fac n = product [1..n]

fac' :: Int -> Int
fac' 0 = 1
fac' n = n * fac'(n-1)

-- Recursion on Lists

product' :: Num a => [a] -> a
product' []     = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Example(++) = [a] -> [a] -> [a]
--        [] ++ ys = ys
--        (x:xs) ++ ys = x : (xs ++ ys) 

-- Multiple Arguments

zip' :: [a] -> [b] -> [(a,b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x,y) : zip xs ys

drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (_:xs) = drop' (n-1) xs

-- Multiple Recursions

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--Mutual Recursions

even' :: Int -> Bool
even' 0 = True
even' n = odd (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even (n-1)

evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds []     = []
odds (_:xs) = evens xs

--Programming Example (QuickSort)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]

-- Advice on recursion

drop'' :: Int -> [a] -> [a]
drop'' 0 xs     = xs
drop'' _ []     = []
drop'' n (_:xs) = drop'' (n-1) xs

-- Exercises

-- Exercise 1

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' []   = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x  = [x] ++ replicate' (n-1) x

(!!!) :: [a] -> Int -> a
(!!!) [] _     = undefined
(!!!) (x:xs) n | n == 0    = x
               | otherwise = (!!!) xs (n-1)
               
elem' :: Eq a => a -> [a] -> Bool
elem' x []     = False
elem' y (x:xs) | y == x    = True
               | otherwise = elem' y xs


-- Exercise 2

merge :: Ord a => [a] -> [a] -> [a]
merge x []                      = x
merge [] y                      = y
merge (x:xs) (y:ys) | x <= y    = x:(merge xs(y:ys))
                    | otherwise = y:(merge (x:xs)ys)
                    
-- Higher Order Functions

--Basic Concepts

twice :: (a -> a) -> a -> a
twice f x = f (f x)

--Map Function

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' f []     = []
map'' f (x:xs) = f x : map'' f xs

-- Filter Function 

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [ x | x <- xs, f x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f [] = []
filter'' f (x:xs) | f x = x : filter'' f xs
                  | otherwise = filter'' f xs

-- Foldr Function

sum'' :: [Int] -> Int
sum'' = foldr (+) 0

product'' :: [Int] -> Int
product'' = foldr (*) 1

and'' :: [Bool] -> Bool
and'' = foldr (&&) True

or'' :: [Bool] -> Bool
or'' = foldr (||) False

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f y [] = y
foldr' f y (x:xs) = f x (foldr' f y xs)

-- Other foldr function

length'' :: [a] -> Int
length'' = foldr (\_ n -> 1 + n) 0

reverse'' :: [a] -> [a]
reverse'' = foldr (\x xs -> xs ++ [x]) []

(++++) :: [a] -> [a] -> [a]
(++++) = foldr (:)

-- Foldl Function

sum''' :: Num a => [a] -> a
sum''' = foldl (+) 0

product''' :: [Int] -> Int
product''' = foldl (*) 1

or''' :: [Bool] -> Bool
or''' = foldl (||) False

and''' :: [Bool] -> Bool
and''' = foldl (&&) True

length''' :: [a] -> Int
length''' = foldl (\n _ -> n + 1) 0

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []     = v
foldl' f v (x:xs) = foldl f (f v x) xs

-- Composition Operator

oddd :: Int -> Bool
oddd = not . even

twicee :: (a -> a) -> a -> a
twicee f = f . f

sumsqreven :: Integral a => [a] -> a
sumsqreven = sum . map (^2) . filter even

-- Other Library Functions

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = and [p x | x <- xs]

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = or [p x | x <- xs]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
   | p x       = x : takeWhile' p xs
   | otherwise = []

-- Programming Example - Binary String Transmitter

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Transmission

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id


concat3 :: [[a]] -> [a]
concat3 [] = []
concat3 (x:xs) = x ++ concat3 xs

replicate' :: Int -> a -> [a]
replicate' 0 _ -> []
replicate' n (x) = [x] ++ replicate' (n-1) x