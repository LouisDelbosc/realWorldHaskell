-- file: Chapter4/Exercice.hs

import Data.Char

-- Part 1
-- exercice 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [_] = Just []
safeInit xs = Just (init xs)

-- exercice 2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith cond (x:xs) = (x : (takeWhile cond xs)) : (splitWith cond (dropWhile cond xs))


-- Part 2
-- exercice 1

asInt_fold :: String -> Int
asInt_fold ('-':xs) = negate (asInt_fold xs)
asInt_fold xs = foldl step 0 xs
  where step acc x = let acc' = acc * 10 + (digitToInt x)
                         -- for overflow case
                      in if (acc' < acc)
                            then error "Overflow"
                            else acc'

-- exercice 2
type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either ('-':xs) = neg (asInt_either xs)
  where neg (Left acc) = Left acc
        neg (Right acc) = Right (negate acc)

asInt_either xs = foldl step (Right 0) xs
  where step (Left acc) _ = Left acc
        step (Right acc) x
            | (acc' < acc) = Left ("Overflow" ++ [x] ++ "")
            | isDigit x = Right (acc * 10 + digitToInt x)
            | otherwise = Left ("non-digit" ++ [x] ++ "")
              where acc' = acc * 10 + digitToInt x

-- exercice 3
myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

-- exercice 4
myTakeWhileRec :: (a -> Bool) -> [a] -> [a]
myTakeWhileRec cond (x:xs)
  | cond x = x : (myTakeWhileRec cond xs)
  | otherwise = []
myTakeWhileRec _ _ = []

myTakeWhileFold :: (a -> Bool) -> [a] -> [a]
myTakeWhileFold cond xs = foldr step [] xs
  where step x xs | cond x = x:xs
                  | otherwise = []

-- exercice 5

-- half working version
-- doesn't work with groupBy (<) [1,2,3,2,0,0,3,3,1,0]]
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy cond xs = foldr step [] xs
  where step x y
              | null y = [[x]]
              | cond x (head (head y)) = (x: head y) : tail y
              | otherwise = [x] : y

-- recursive version
recGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
recGroupBy f (x:xs) = let (ys, zs) = span (f x) xs
                       in (x : ys) : recGroupBy f zs
recGroupBy _ _ = []

-- exercice 6
myAny :: (a -> Bool) -> [a] -> Bool
myAny cond x = foldr (\x y -> cond x || y) False x

cycle' :: [a] -> [a]
cycle' list = foldr step [] [1..]
  where step _ ys = list ++ ys

words' :: String -> [String]
words' list = foldr step [] (zip list (tail list ++ " "))
  where step (a,b) gs 
             | a == ' ' = gs
             | b == ' ' = [a]:gs
             | otherwise = (a:(head gs)):(tail gs)
