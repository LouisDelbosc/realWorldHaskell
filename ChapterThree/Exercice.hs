-- file: ChapterTree/Exercice.hs

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

-- Debut exercice
compute :: [a] -> Int

compute (x:xs) = 1 + compute xs
compute [] = 0


-- moyenne
mean :: [Double] -> Double

mean list = snd (innerFunction list)/ fst (innerFunction list)
          where innerFunction [] = (0,0)
                innerFunction (x:xs) = (fst (innerFunction xs) +1, snd(innerFunction xs) + x)

-- Palindrome
palindrome :: [a] -> [a]

palindrome list = list ++ innerFunction list
                where innerFunction [] = []
                      innerFunction (x:xs) = (innerFunction xs) ++ [x]


isPalindrome ::Eq a => [a] -> Bool

isPalindrome list = (list == (reverseList list))
                  where reverseList [] = []
                        reverseList (x:xs) = (reverseList xs) ++ [x]

-- intersperse dans une liste
inter :: a -> [[a]] -> [a]

inter n [] = []
inter _ (x:[]) = x
inter n (x:xs) = x ++ n : (inter n xs)

-- arbre

heightTree Empty = 0
heightTree (Node _ b c) = 1 + max (heightTree b) (heightTree c)
