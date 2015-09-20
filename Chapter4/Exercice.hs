-- file: Chapter4/Exercice.hs

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
