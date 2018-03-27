module Playground
where

    import Data.List

    sum_ :: Num a => [a] -> a


    split :: [a] -> ([a], [a])
    split myList = splitAt ((length myList + 1) `div` 2) myList

    sum_ [] = 0
    sum_  [x] = x 
    sum_ xs = sum_ zs + sum_ ys where
        xst = split xs
        zs = fst xst
        ys = snd xst

    data List a = Cons a ( List a ) | Nil deriving ( Show )

    fromList :: List a -> [a]   
    fromList (Cons x xs)  = x : fromList xs  
    fromList Nil = []

    toList :: [a] -> List a
    toList = foldr Cons Nil

    {- Ugly as hell
    data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a)) deriving (Show)
    -}
    

    mySecond :: [a] -> a
    mySecond xs = if null (tail xs)              
                    then error "list too short"              
                    else head (tail xs)

    safeSecond :: [a] -> Maybe a
    safeSecond ( _ : x : _ ) = Just x
    safeSecond _ = Nothing    

    defaultReserve = 10.0
    
    --Function to return the amount left after consuming a given quantity 
    --from the stock provided that the reserve is never exceeded
    consume :: Double -> Double -> Maybe Double   
    consume quantity stock = let 
                                newStock = stock - quantity
                                reserve = defaultReserve
                             in 
                                if stock < reserve || newStock < reserve
                                    then Nothing
                                    else Just newStock

    consume2 :: Double -> Double -> Maybe Double
    consume2 quantity stock = if stock < reserve || newStock < reserve
                                    then Nothing
                                    else Just newStock
                              where     
                                    reserve = defaultReserve
                                    newStock = stock - quantity  

    parity :: Int -> String
    parity n = if odd n then "Odd" else "Even"

    choice5 :: (Int, Int, Int) -> String
    choice5 (x, y, z) = case (x, y, z) of 
                            (_, 5, _) -> "I choose second"
                            (5, _, _) -> "I choose first"
                            (_, _, 5) -> "I choose third"
                            _ -> "I choose none"

    --End of Chapter #3
    --Exercise 1,2 
    len :: [a] -> Int
    len = foldr (\ x -> (+) 1) 0

    --Excercise 3
    mean :: Fractional a => [a] -> a
    mean xs = sumxs / lenxs where
                    sumxs = sum xs
                    lenxs = fromIntegral . length $ xs
   
    mean_ :: (Fractional a) => [a] -> a
    mean_ (x:xs) = let
                        lenxs =  fromIntegral . length $ xs
                   in
                        (lenxs * mean_ xs + x) / (lenxs + 1) 
    mean_ [] = 0


    --Exercise 4                
    makePalindrome :: [a] -> [a]
    makePalindrome xs =  xs ++ reverse xs

    --Exercise 5
    --Correct but inefficient ... it kills ghc for lists over 10000 elements
    checkPalindromeBad :: (Eq a) => [a] -> Bool
    checkPalindromeBad [] = True
    checkPalindromeBad [x] = True
    checkPalindromeBad (x:xs) = (x == y) && checkPalindromeBad ys
                             where   
                                y = last xs
                                ys = take (length xs - 1)  xs

    --Correct and very efficient (probably optumal)
    checkPalindrome :: (Eq a) => [a] -> Bool
    checkPalindrome xs = xs == reverse xs

    --Exercise 6
    sortByLen :: (Show a) => [[a]] -> [[a]]
    sortByLen = sortOn length 
    -- The above is better as it computes length just once per each element, 
    -- as per documentation: Schwrtzian transform
    sortByLen2 :: (Show a) => [[a]] -> [[a]]
    sortByLen2 = sortBy (\ xs ys -> compare (length xs) (length ys))

    --Exercise 7
    intersperse_ :: (Show a) => a -> [[a]] -> [a]
    intersperse_ x xss | null xss = []
                       | length xss == 1 = head xss 
                       | otherwise  = foldr join [] xss
                                                where join xs ys = if null ys then xs
                                                                   else xs ++ [x] ++ ys
    -- And the much cleaner version that didn't occur to me: 
    intersperse2_ :: (Show a) => a -> [[a]] -> [a]
    intersperse2_ _ [] = []
    intersperse2_ _ [xs] = xs
    intersperse2_ x (xs:xss) = xs ++ [x] ++ intersperse2_ x xss         
    
    --Exercise 8
    data Tree a = Node a (Tree a) (Tree a) | Empty deriving ( Show )   
    height :: Tree a -> Int
    height Empty = 0
    height (Node _ left right) = 1 + max (height left) (height right)
