removeLast :: [a] -> [a]
removeLast xs = reverse (tail (reverse xs))

removeElem :: Int -> [a] -> [a]
removeElem n xs = removeLast (take n xs) ++ (drop n xs)

removeFirstAndLast :: [a] -> [a]
removeFirstAndLast xs = removeLast (removeElem 1 xs)

abs' :: Integer -> Integer
abs' n = if n>=0 then n else -n

likeHaskell :: Int -> String
likeHaskell n = if n < 3 then "I don't like it" else if n < 7 then "It's manageable" else "It's really great!"

abs :: Int -> Int 
abs n | n >= 0 = n
      | otherwise = -n 

likeHaskell' :: Int -> String
likeHaskell' n | n < 3 = "I don't like it" 
               | n < 7 = "It's manageable"
               | otherwise = "It's really great!"


allen :: Int -> Int -> Bool
allen m n | m > n && m < (2*n) = True
          | otherwise = False

notB :: Bool -> Bool
notB False = True
notB True = False

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (x:xs) = False

andB :: Bool -> Bool -> Bool
andB True True = True
andB True False = False
andB False True = False
andB False False = False

andB' :: Bool -> Bool -> Bool
andB' True True = True
andB' _ _ = False

andB'' :: Bool -> Bool -> Bool
andB'' True b = b
andB'' False _ = False

orB :: Bool -> Bool -> Bool
orB False False = False
orB _ _ = True

isTrue :: Bool -> Bool
isTrue True = True
isTrue False = error "not True"

fst :: (a,b) -> a 
fst (x,y) = x

fst' :: (a,b) -> a
fst' (x,_) = x

snd :: (a,b) -> b
snd (_,y) = y

third :: (a,b,c) -> c
third (_,_,z) = z

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2,y2) = (x1+x2,y1+y2)

-- OnLists
isEmpty'' :: [a] -> Bool
isEmpty'' [] = True
isEmpty'' (_:_) = False

sndElem :: [a] -> a
sndElem (_:x:_) = x
                       
thirdElem :: [a] -> a
thirdElem (_:_:x:_) = x

-- Case Expressions
isEmpty2 :: [a] -> Bool
isEmpty2 x = case x of [] -> True
                       (_:_) -> False

--Lambda Expressions
double :: Int -> Int
double x = 2 * x

double' :: Int -> Int
double' = \x -> 2 * x

-- Lambda Expressions (Several Input Variables)
mult :: Int -> Int -> Int
mult x y = x * y

mult' :: Int -> Int -> Int
mult' = \x y -> x * y

mult'' :: Int -> (Int -> Int)
mult'' = \x -> (\y -> x*y)

alwaysZero :: Char -> Int
alwaysZero = \_ -> 0

apply :: (a->b) -> a -> b
apply f x = f x

--Operators & Sections
square :: Int -> Int
square = (^2)

reci :: Fractional a => a -> a
reci = (1/)

--Exercises 1

third' :: [a] -> a
third' x = head(reverse (tail (take 3 x)))

third'' :: [a] -> a
third'' xs = xs !! 2

third''' :: [a] -> a
third''' (_:_:x:_) = x

--Exercises 2
safetail :: [a] -> [a]
safetail x = if isEmpty x then [] else tail(x)

safetail' :: [a] -> [a]
safetail' x | isEmpty[x] = []
            | otherwise = tail(x)
            
safetail'' :: [a] -> [a]
safetail'' x = case isEmpty x of True -> []
                                 False -> tail x
                                 
                                