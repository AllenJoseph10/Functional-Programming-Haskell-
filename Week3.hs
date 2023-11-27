import Data.Char

-- Dependent Generators

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs ]

firsts :: [(a,b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs ]

-- Guards

factors :: Int -> [Int]
factors xs = [x | x <- [1..xs], xs `mod` x == 0]

prime :: Int -> Bool
prime xs = factors xs == [1,xs]

primes :: Int -> [Int]
primes xs = [x | x <- [2..xs], prime x]

-- The Zip Function (Pre - defined)
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y |(x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- String Comprehensions

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x'] 

lowers :: String -> Int
lowers xs = length [ x | x <- xs, x >= 'a' && x <= 'z']

-- The Caesar Cipher

let2int :: Char -> Int
let2int xs = ord xs - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n xs | isLower xs = int2let ((let2int xs + n) `mod` 26)
           | otherwise = xs

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs]

-- Frequency Tables

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]


percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs
           
-- Cracking the cipher

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
     factor = head (positions (minimum chitab) chitab)
     chitab = [chisqr (rotate n table') table | n <- [0..25]]
     table' = freqs xs

-- Exercises
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(a,b,c) | a <- ns , b <- ns , c <- ns , a^2 + b^2 == c^2]
  where ns = [1..n]
  
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [y | y <- [1..n], sum (divisors y) == y] 

chisqr' :: [Int] -> [Int] -> Int
chisqr' xs ys = sum [(x*y) | (x,y) <- zip xs ys]


