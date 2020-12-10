import Control.Applicative -- for liftA2
import System.Random -- for random
import Data.List --for splitAt
import Control.Monad --for when
import qualified Data.ByteString.Lazy as B  --for lazy
import qualified Data.ByteString as S  -- for strict
import qualified Foldable as F  --for foldable

-- get last
getLast xs = foldr (const id) xs
getLast' xs = last xs

-- get second to last
myButLast = last . init
myButLast' xs = reverse xs !! 1
lastbut1 :: Foldable f => f a -> a
lastbut1 = fst . foldl (\(a,b) x -> (b,x)) (err1,err2)
  where
    err1 = error "lastbut1: Empty list"
    err2 = error "lastbut1: Singleton" 

--list comprehensions are like do blocks
bList = [ x | x <- [1..50], '7' `elem` show x ] 


--when monad (only executes final arg when the first arg is true)
isCool s = when (s == True) (putStrLn "cool")

--quick sort
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted

--adjacent pairs, bind two lists together. (midterm 1)
adjacentPairs :: [a] -> [(a, a)]
adjacentPairs xs = zipWith (,) xs [x | x <- (tail xs) ++ ([head xs])]

--currying 
curry3 f x y z = f (x, y, z) 
uncurry3 f (x, y, z) = f x y z


---------------Key Value dictionary stuff
--lookup a value with a key with error output
lookupWithError :: Eq a => a -> [(a, p)] -> p
lookupWithError a ps = 
    case lookup a ps of
        Nothing -> error "key not found"
        Just v -> v


-----------------------filtering
--find sum of all odd squares under 100000 
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum' :: Integer  --above with function composition
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

------------------------data
--record syntax
data Joe = Joe {name :: String, age :: Int} deriving(Show)
newJoe = Joe "nnnnn" 80
getJoeName = name newJoe

--polymorphic record syntax (type constructor)
data Car a b c = Car { company :: a, model :: b, year :: c} deriving (Show)

tellCar :: (Show a) => Car String String a -> String  --this makes car always be defined as Car abc
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Car' = Car' { company' :: String, model' :: String, year' :: Int} deriving (Show)
tellCar' :: Car' -> String 
tellCar' (Car' {company' = c, model' = m, year' = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

--vector datatype
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

--datatype deriving various type classes
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday 
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
satVsFri = Saturday > Friday --Ord
getMinBound = minBound :: Day --Bounded
getAllDays = [minBound .. maxBound] :: [Day]  --Bounded

--recursive data structures
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
--same as above in record form
data List' a = Empty' | Cons' { listHead :: a, listTail :: List' a} deriving (Show, Read, Eq, Ord)
makeList = 3 `Cons` (4 `Cons` (5 `Cons` Empty))
--same but with infixr
infixr 5 :-: --five is its precedence
data List'' a = Empty'' | a :-: (List'' a) deriving (Show, Read, Eq, Ord)
--more infixr for custom list data structure.
infixr 5  .++  
(.++) :: List'' a -> List'' a -> List'' a   
Empty'' .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)


-------------------Tree Stuff
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

buildTree = 
    let nums = [8,6,4,1,7,3,5]    
    in  foldr treeInsert EmptyTree nums

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

--import qualified Foldable as F  
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  

testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )  
--ghci> F.foldl (+) 0 testTree  
--42  
--ghci> F.foldl (*) 1 testTree  
--64800  
--ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree  --Any reference from Monoid section
--True  
-----------------classes
class YesNo a where  
    yesno :: a -> Bool

instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True
instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True
instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True
    
-----------------instances
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"

--making instances for type constructors without concrete types
data YeaMaybe a = YeaNothing | YeaJust a
instance (Eq m) => Eq (YeaMaybe m) where  
    YeaJust y == YeaJust x = x == y  
    YeaNothing == YeaNothing = True  
    _ == _ = False

-----------------IO basics
-- get a line and then print it ===================
-- main = do  
--     putStrLn "Hello, what's your name?"  
--     name <- getLine  
--     putStrLn ("Hey " ++ name ++ ", you rock!")

--using let in IO =================================
-- main = do
--     putStrLn "What's your first name?"
--     firstName <- getLine
--     putStrLn "What's your last name?"
--     lastName <- getLine
--     let bigFirstName = map toUpper firstName
--         bigLastName = map toUpper lastName
--     putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?

--calling another function in IO ==================
-- main = do   
--     line <- getLine  
--     if null line  
--         then return ()  
--         else do  
--             putStrLn $ reverseWords line  
--             main  
-- reverseWords :: String -> String  
-- reverseWords = unwords . map reverse . words

--sequence ========================================
-- main = do  
--     rs <- sequence [getLine, getLine, getLine]  
--     print rs

--sequence to create IO action out of pure (with IO return)===================
printAList = sequence (map print [1,2,3,4,5])

--using mapM to create IO action ===================
printAList' = mapM_ print [1,2,3]  --dont have IO return
printAList'' = mapM print [1,2,3]  --include IO return

--looping IO do blocks ===============================
-- import Control.Monad  
-- import Data.Char  
-- main = forever $ do  
--     putStr "Give me some input: "  
--     l <- getLine  
--     putStrLn $ map toUpper l

--cool use of forM which is flipped mapM this is cool ===================
-- import Control.Monad  
-- main = do   
--     colors <- forM [1,2,3,4] (\a -> do  
--         putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
--         color <- getLine  
--         return color)  
--     putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
--     mapM putStrLn colors

--getContents to read a file ============================================
--the outside function filters long lines out
-- main = do  
--     contents <- getContents  
--     putStr (shortLinesOnly contents)  
--   
-- shortLinesOnly :: String -> String  
-- shortLinesOnly input =   
--     let allLines = lines input  
--         shortLines = filter (\line -> length line < 10) allLines  
--         result = unlines shortLines  
--     in  result

--same thing as above with function composition ==========================
-- main = interact $ unlines . filter ((<10) . length) . lines


--check if is a palindrome constantly ====================================
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where   isPalindrome xs = xs == reverse xs
--using interact (String -> String) -> IO
--using interact can also accept files.
--main = interact respondPalindromes  


--specify file in the do block ===============================================
--hGetContents basically reads file and the mode
-- import System.IO  
--   
-- main = do  
--     handle <- openFile "girlfriend.txt" ReadMode  
--     contents <- hGetContents handle  
--     putStr contents  
--     hClose handle



-------------------IO Args
--basic argument accepting
-- import System.Environment   
-- import Data.List  
--   
-- main = do  
--    args <- getArgs  
--    progName <- getProgName  
--    putStrLn "The arguments are:"  
--    mapM putStrLn args  
--    putStrLn "The program name is:"  
--    putStrLn progName



-------------------random
--import System.Random
--generate a random Int
getRanNum = random (mkStdGen 10) :: (Int, StdGen)

--easy way to generate a sequence of random numbers.
get5Rand = take 5 $ randoms (mkStdGen 11) :: [Int]

--custom "randoms" that returns the generators
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

-- coin tosser (boolean random three tuple gen)
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)
-- *Main> threeCoins (mkStdGen 12)
-- (False,True,False)

--use randomRs to generate from a range of values
getRand10String = take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]

--with IO and getStdGen (IO stdGen)
-- main = do  
--     gen <- getStdGen  
--     putStr $ take 20 (randomRs ('a','z') gen)

--using splitAt
--generates infinite string and then takes from it
--to make illusion that its generating two different strings
-- import System.Random  
-- import Data.List  
-- main = do  
--     gen <- getStdGen  
--     let randomChars = randomRs ('a','z') gen  
--         (first20, rest) = splitAt 20 randomChars  
--         (second20, _) = splitAt 20 rest  
--     putStrLn first20  
--     putStr second20

----------program to generate a random number and then compare it to user input.
-- import System.Random  
-- import Control.Monad(when)  
--   
-- main = do  
--     gen <- getStdGen  
--     askForNumber gen  
--   
-- askForNumber :: StdGen -> IO ()  
-- askForNumber gen = do  
--     let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  
--     putStr "Which number in the range from 1 to 10 am I thinking of? "  
--     numberString <- getLine  
--     when (not $ null numberString) $ do  
--         let number = read numberString  
--         if randNumber == number   
--             then putStrLn "You are correct!"  
--             else putStrLn $ "Sorry, it was " ++ show randNumber  
--         askForNumber newGen 










-------------bytestrings
getAZ = B.pack [97..122]  
-- "abcdefghijklmnopqrstuvwxyz"






-----------------Reverse Polish notation Calc
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ** x):ys
            foldingFunction (x:xs) "ln" = log x:xs
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString:xs
-- solveRPN "2 3 +" 
-- 5








----------Functor Examples "Apply stuff in the box"
--Functor Laws
-- The first functor law states that if we map the id function over a functor, 
-- the functor that we get back should be the same as the original functor.
-- fmap id (Just 3)  >>>> Just 3 

--The second law says that composing two functions and then mapping the resulting 
-- function over a functor should be the same as first mapping one function over the 
-- functor and then mapping the other one.
-- fmap (f . g) = fmap f . fmap g


--All about (->)
-- (-> r a ) = type r -> a
-- functor only accepts type constructors with a single param which is why its like ((->) r) 
-- instance Functor ((->) r) where
--     fmap = (.)
--     fmap f g = (\x -> f (g x))


--IO
-- instance Functor IO where
--     fmap f action = do
--         result <- action
--         return (f result)
-- main = do 
--           line <- fmap reverse getLine
--           line <- fmap (intersperse '-' . reverse) getLine -- another thing you can do with fmap ( . map toUpper can be added to reverse)
--           putStrLn $ "You said " ++ line ++ " backwards!"  
--           putStrLn $ "Yes, you really said" ++ line ++ " backwards!"










----------Applicative Examples "Apply box to another box"
-- basic use case, use fmap to turn the value inside of a box/datatype/etc. into a function
-- apply this function to another box/datatype/etc. to get a final value wrapped by the box.

---------------liftA2
liftJust = liftA2 (:) (Just 3) (Just [4])  
--Just [3,4]

--liftA2 with monads and applicative
liftJust' = (:) <$> Just 3 <*> Just [4]  
-- Just [3,4]

--applicative and foldr
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs

sequenceA'' :: (Applicative f) => [f a] -> f [a]  
sequenceA'' = foldr (liftA2 (:)) (pure [])
-- sequenceA [Just 3, Just 2, Just 1]  
-- Just [3,2,1]  
-- sequenceA [(+3),(+2),(+1)] 3  
-- [6,5,4]  
seqATest = sequenceA [[1,2,3],[4,5,6]]  
seqATestListC = [[x,y] | x <- [1,2,3], y <- [4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  






--IO
-- instance Applicative IO where  
--     pure = return  
--     a <*> b = do  
--         f <- a  
--         x <- b  
--         return (f x)
--unpacks the boxes and the combines them and then re wraps them with return.

--uses applicative to combine two getLine IO actions
-- main = do
--     a <- (++) <$> getLine <*> getLine
--     putStrLn $ "The two lines concatenated turn out to be: " ++ a



------ziplist
-- instance Applicative ZipList where  
--         pure x = ZipList (repeat x)  
--         ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
------------has dummy function in zipWith so the new value can be wrapped correctly.
-- eg > 
breakingDownZipApp = zipWith (\f x -> f x) [(+2),(+3)] [3,4,5]
-- [5,7]


--with the Ziplist applicative [(+3),(*2)] <*> [1,2] goes from [4,5,2,4] to [4,4]
--ziplist applicative allows us to have multiple matching hits on a list.

--getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
-- [101,102,103]
-- getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
-- [101,102,103]
-- getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
-- [5,3,3,4]
-- getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
-- [('d','c','r'),('o','a','a'),('g','t','t')]


--Just
-- instance Applicative Maybe where
--     pure = Just -- like return in IO
--     Nothing <*> _ = Nothing
--     (Just f) <*> something = fmap f something
-- Just (+3) <*> Just 9  >>>>> Just 12
-- pure(+3) <*> Just 9  >>>>> Just 12
-- pure (+) <*> Just 3 <*> Just 5 >>>>> Just 8

--using infix fmap with applicative
-- (++) <$> Just "johntra" <*> Just "volta"      >>>>>>>> Just "johntravolta"
-- pure (++) <*> Just "johntra" <*> Just "volta" >>>>>>>> Just "johntravolta"“[(*0),(+100),(^2)] <*> [1,2,3]”


--applicative lists
--combing lists, using functions on lists, can replace list comprehensions
-- [(*0),(+100),(^2)] <*> [1,2,3] >>>>>> [0,0,0,101,102,103,1,4,9]
-- [(+),(*)] <*> [1,2] <*> [3,4]  >>>>>> [4,5,5,6,3,4,6,8]  
-- (++) <$> ["ha","heh","hmm"] <*> ["?","!","."] >>>>>> ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

listCV = [ x*y | x <- [2,5,10], y <- [8,10,11]]
listAppV = pure (*) <*> [2,5,10] <*> [8,10,11]
listAppV' = (*) <$> [2,5,10] <*> [8,10,11]\

-------------monoids
--the monoid class
-- class Monoid m where  
--     mempty :: m  
--     mappend :: m -> m -> m  
--     mconcat :: [m] -> m  
--     mconcat = foldr mappend mempty  

-- mempty: A polymorphic constant, kind of like minBound from Bounded. mempty represents the identity value for a particular monoid.
-- mappend: takes two monoid values and returns a third. Kinda like ++ but not really.
-- mconcat: takes a list of monoid values and reduces them to a single value by doing mappend between the list's elements. 

-- monoid laws
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- lists are monoids
--instance Monoid [a] where  
--    mempty = []  
--    mappend = (++) 
--Notice that instance Monoid [a] is not instance Monoid [] > Monoid requires a concrete type for an instance.

-- ghci> [1,2,3] `mappend` [4,5,6]  
-- [1,2,3,4,5,6]  
-- ghci> ("one" `mappend` "two") `mappend` "tree"  
-- "onetwotree"  
-- ghci> "one" `mappend` ("two" `mappend` "tree")  
-- "onetwotree"  
-- ghci> "one" `mappend` "two" `mappend` "tree"  
-- "onetwotree"  
-- ghci> "pang" `mappend` mempty  
-- "pang"  
-- ghci> mconcat [[1,2],[3,6],[9]]  


--example (with newtype, which is basically data)
--newtype Product a =  Product { getProduct :: a }  
--    deriving (Eq, Ord, Read, Show, Bounded)  

--instance Num a => Monoid (Product a) where  
--    mempty = Product 1  
--    Product x `mappend` Product y = Product (x * y)  

--ghci> getProduct $ Product 3 `mappend` Product 9  
--27  

--newtype Any = Any { getAny :: Bool }  
--    deriving (Eq, Ord, Read, Show, Bounded)  

--instance Monoid Any where  
--        mempty = Any False  
--        Any x `mappend` Any y = Any (x || y)  
--ghci> getAny $ Any True `mappend` Any False  
--True  
--ghci> getAny $ mempty `mappend` Any True  
--True  
--ghci> getAny . mconcat . map Any $ [False, False, False, True]  
--True  
--ghci> getAny $ mempty `mappend` mempty  
--False  


--------Foldadable.
--foldable foldr is polymorphic, it doesnt care about type. foldr from prelude only works on lists.
--ghci> F.foldl (+) 2 (Just 9)  
--11  
--ghci> F.foldr (||) False (Just True)  
--True  




------------monads
-- first monad law: The first monad law states that if we take a value, put it in a default context with return 
-- and then feed it to a function by using >>=, it's the same as just taking the value and applying the function to it. 
-- (left Identity) || return x >>= f is the same as f x

-- second monad law: The second law states that if we have a monadic value and we use >>= to feed it to return,
-- the result is our original monadic value. 
-- (Right identity) || m >>= return is no different than just m

-- third monad law: The final monad law says that when we have a chain of monadic function applications with >>=,
-- it shouldn't matter how they're nested. 
-- (associativity) || (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)


--class Monad m where  
--    return :: a -> m a   --same as pure 
--    (>>=) :: m a -> (a -> m b) -> m b   
--    (>>) :: m a -> m b -> m b  
--    x >> y = x >>= \_ -> y   
--    fail :: String -> m a  
--    fail msg = error msg  -- fail

--------example of Maybe Monad
--instance Monad Maybe where  
--    return x = Just x  
--    Nothing >>= f = Nothing  
--    Just x >>= f  = f x  
--    fail _ = Nothing  

--ghci> return "WHAT" :: Maybe String  
--Just "WHAT"  
--ghci> Just 9 >>= \x -> return (x*10)  
--Just 90  
--ghci> Nothing >>= \x -> return (x*10)  
--Nothing  


--sample program with maybe to check bird landings

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  

ghci> landRight 1 (0,0) >>= landLeft 2  
Just (2,1)  

ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
Just (2,4)  

(without monad)
routine :: Maybe Pole  
routine = case landLeft 1 (0,0) of  
    Nothing -> Nothing  
    Just pole1 -> case landRight 4 pole1 of   
        Nothing -> Nothing  
        Just pole2 -> case landLeft 2 pole2 of  
            Nothing -> Nothing  
            Just pole3 -> landLeft 1 pole3 
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    second <- landRight 2 first  
    landLeft 1 second  

---------------example of [] monad
-- monad in this case takes care of non-determinism 
-- [] is similar to nothing in this case
instance Monad [] where  
    return x = [x]  --makes a list that has only that one value as its result. 
    xs >>= f = concat (map f xs)  
    fail _ = []  
--example
-- ghci> [3,4,5] >>= \x -> [x,-x]  
--[3,-3,4,-4,5,-5] 

--non-determinism
-- this is kinda like a nested for loop.
--ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  
--[(1,'a'),(1,'b'),(2,'a'),(2,'b')]  


-------MonadPlus
--The MonadPlus type class is for monads that can also act as monoids.
class Monad m => MonadPlus m where  
    mzero :: m a  
    mplus :: m a -> m a -> m a  

instance MonadPlus [] where  
    mzero = []  
    mplus = (++) 

--monadPlus in practice
guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero  
ghci> guard (5 > 2) :: Maybe ()  
Just ()  
ghci> guard (1 > 2) :: Maybe ()  
Nothing  
ghci> guard (5 > 2) :: [()]  
[()]  
ghci> guard (1 > 2) :: [()]  
[] 
--checks for failure, fails well
ghci> guard (5 > 2) >> return "cool" :: [String]  
["cool"]  
ghci> guard (1 > 2) >> return "cool" :: [String]  
[]  

-----------------chess example
type KnightPos = (Int,Int)  
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')  
-- filter version
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]  

in3M start = return start >>= moveKnight >>= moveKnight >>= moveKnight  

in3Do :: KnightPos -> [KnightPos]  
in3Do start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second  

canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` inM start  



--------------------------------------WRITER
--the Writer monad is for values that have another value attached that acts as a sort of log value. 
-- Writer allows us to do computations while making sure that all the log values are combined into one 
-- log value that then gets attached to the result.
--Basically, this 
isBigGang x = (x > 9, "Compared gang size to 9.")  

--basic example of stringing together logs 
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog (x,log) f = 
    let (y,newLog) = f x 
    in (y,log ++ newLog)  
--f outputs another log, which is appended to the original log
-- put isBigGang as f and it will work correctly as f outputs a log


--using monoids funcs with monoids list and bytestring 
--to replace previous "apply log"
ghci> [1,2,3] `mappend` [4,5,6]  
[1,2,3,4,5,6]  
ghci> B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97]  
Chunk "chi" (Chunk "huahua" Empty) 


applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = 
        let (y,newLog) = f x 
        in (y,log `mappend` newLog) 

import Data.Monoid  
  
type Food = String  
type Price = Sum Int  
  
addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30) 

ghci> ("beans", Sum 10) `applyLog` addDrink  
("milk",Sum {getSum = 35})  
ghci> ("jerky", Sum 25) `applyLog` addDrink  
("whiskey",Sum {getSum = 124})  
ghci> ("dogmeat", Sum 5) `applyLog` addDrink  
("beer",Sum {getSum = 35})  
