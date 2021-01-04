-- Lists Type ----------------------------------------------------------------<

-- Lists Transformation ------------------------------------------------------<

-- Add an element to beginning of a list
addToListLeft :: a -> [a] -> [a]
addToListLeft x xs = x : xs

-- Add an element to ending of a list
addToListRight :: a -> [a] -> [a]
addToListRight x [] = [x]
addToListRight k (x:xs) = x : addToListRight k xs

-- Delete an element from list
delFromList :: Eq a => a -> [a] -> [a]
delFromList _ [] = []
delFromList k (x:xs)
        | k == x = delFromList k xs
        | otherwise = x : delFromList k xs

-- Reverse our list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- Find our list length
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Split our list in index
splitListByIndex :: Int -> [a] -> ([a], [a])
splitListByIndex n xs
        | n <= 0 = ([], xs)
        | otherwise = splitByIndex' n xs
                where
                        splitByIndex' :: Int -> [a] -> ([a], [a])
                        splitByIndex' _ [] = ([], [])
                        splitByIndex' 1 (x:xs) = ([x], xs)
                        splitByIndex' m (x:xs) = (x:xs', xs'')
                                where
                                        (xs', xs'') = splitByIndex' (m-1) xs

-- Split list by halfs
splitListHalfs :: [a] -> ([a], [a])
splitListHalfs [] = ([], [])
splitListHalfs our_list = splitListByIndex ( div ((length our_list) + 1) 2) our_list

-- Print all sublists from our list
listCheckSublists :: [a] -> [[a]]
listCheckSublists [] = [[]]
listCheckSublists (x:xs) = sublists' [] (x:xs) ++ listCheckSublists xs
        where
                sublists' xs [] = []
                sublists' xs (x':xs') = (xs ++ [x']) : (sublists' (xs ++ [x']) xs')
