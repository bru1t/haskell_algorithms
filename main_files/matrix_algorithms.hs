-- Matrix Type ---------------------------------------------------------------<

-- Matrix Transformation -----------------------------------------------------<

-- Delete line from our matrix
matrixDelLine :: Int -> [a] -> [a]
matrixDelLine 1 (x:xs) = xs
matrixDelLine i (x:xs) = [x] ++ matrixDelLine (i-1) xs

-- Delete column from our matrix
matrixDelColumn :: Num a => Int -> [[a]] -> [[a]]
matrixDelColumn i x = [ c | c <- [ matrixDelLine i q | q <- x ]]

-- Matrix multiplication
matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiply a b = map (mult [] b) a
       where
              mult xs [] _ = xs
              mult xs _ [] = xs
              mult [] (x':xs') (y:ys) = mult (map (y *) x') xs' ys
              mult xs (x':xs') (y:ys) = mult (zipWith (\u v -> u + v * y) xs x') xs' ys

-- Forms a row of a transposed matrix
frstrow :: [[Float]] -> [Float]
frstrow [] = []
frstrow (x:xs) = head x : frstrow xs

-- For deleting a column (removes the selected item from the row)
delColVS :: [Float] -> Int -> [Float]
delColVS (x:xs) y = if y==1 then xs else x : delColVS xs (y-1)

-- Removing the selected column
delCol :: [[Float]] -> Int -> [[Float]]
delCol [] _ = []
delCol (x:xs) y = delColVS x y : delCol xs y

-- Basic transpose function
transp :: [[Float]] -> [[Float]]
transp [[]] = [[]]
transp (x:xs) = if (tail x == []) then [frstrow (x:xs)]
else frstrow (x:xs) : transp(delCol (x:xs) 1)

-- Removes the selected row
delRow :: [[Float]] -> Int -> [[Float]]
delRow (x:xs) y = if y==1 then xs else x:delRow xs (y-1)

-- Determinant function
det :: [[Float]] -> Float
det [[x]] = x
det (x:xs) = sum [ (-1)^(1+j) * x !! (j-1) * det ( delCol xs j ) | j <- [1..length x]]
