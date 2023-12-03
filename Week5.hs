(&&&) :: Bool -> Bool -> Bool
False &&& _ = False
True  &&& x = x

conj :: Bool -> Bool -> Bool
conj False False = False
conj False True = True
conj True True = True
conj True False = False

data BW = Black | White

bw2bool :: BW -> Bool
bw2bool Black = False
bw2bool White = True

bool2bw :: Bool -> BW
bool2bw False = Black
bool2bw True = White

bw2bool' :: BW -> Bool
bw2bool' Black = True
bw2bool' White = False

bool2bw' :: Bool -> BW
bool2bw' True = Black
bool2bw' False = White

data Bit = Zero | One

bit2bool :: Bit -> Bool
bit2bool Zero = False
bit2bool One = True

bool2bit :: Bool -> Bit
bool2bit False = Zero
bool2bit True = One

bit2bool' :: Bit -> Bool
bit2bool' Zero = True
bit2bool' One = False

bool2bit' :: Bool -> Bit
bool2bit' False = One
bool2bit' True = Zero

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
               deriving (Show, Read, Eq, Ord, Enum)
               
data WeekDay' = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
                deriving (Show, Read, Eq, Ord, Enum)
                

dividedby :: Int -> Int -> Maybe Int
x `dividedby` y = if y == 0 then Nothing else Just (x `div` y)

adde :: Maybe Int -> Maybe Int -> Maybe Int
adde Nothing  Nothing  = Nothing
adde Nothing  (Just y) = Nothing
adde (Just x) Nothing  = Nothing
adde (Just x) (Just y) = Just (x + y)

adde' :: Maybe Int -> Maybe Int -> Maybe Int
adde' (Just x) (Just y) = Just (x+y)
adde' _       _         = Nothing

adde'' :: Maybe Int -> Maybe Int -> Maybe Int
adde'' xm ym = case xm of
                Nothing -> Nothing
                Just x  -> case ym of
                            Nothing -> Nothing
                            Just y  -> Just (x+y)

firstPosition :: Eq a => a -> [a] -> Maybe Int
firstPosition x []     = Nothing
firstPosition x (y:ys)
           | x == y    = Just 0
           | otherwise = case firstPosition x ys of
                           Nothing -> Nothing
                           Just n  -> Just (n+1)

testFirstPosition :: Eq a => a -> [a] -> Bool
testFirstPosition x ys =  case firstPosition x ys of
                           Nothing -> and [ ys !! i /= x | i <- [0 .. length ys - 1]]
                           Just n  -> ys !! n == x

-- Either type constructor

data Either a b = Left a | Right b

-- And type constructor

data And a b = Both a b

data MainDish = Chicken | Vegetarian | Pasta
data Dessert = IceCream | Cake | Fruit
data Drink = Tea | Coffee | Beer

type SaverMenu = Either (And MainDish Dessert) (And MainDish Drink)

type SaverMenu' = And MainDish (Either Dessert Drink)

prime :: SaverMenu -> SaverMenu'
prime (Left (Both m d)) = Both m (Left  d)
prime (Right(Both m d)) = Both m (Right d)

unprime :: SaverMenu' -> SaverMenu
unprime (Both m (Left  d)) = Left (Both m d)
unprime (Both m (Right d)) = Right(Both m d)

data List a = Nil | Cons a (List a)

Nil :: List a 
Cons :: a -> List a -> List a 

nativelist2ourlist :: [a] -> List a
nativelist2ourlist [] = Nil
nativelist2ourlist (x:xs) = Cons x (nativelist2ourlist xs)

ourlist2nativelist :: List a -> [a]
ourlist2nativelist Nil = []
ourlist2nativelist (Cons x xs) = x:ourlist2nativelist xs

append' :: List a -> List a -> List a
append' Nil ys = ys
append' (Cons x xs) ys = Cons x (append' xs ys)

rev :: List a -> List a -> List a
rev Nil = Nil
rev (Cons x xs) = rev xs `append` (Cons x Nil)
