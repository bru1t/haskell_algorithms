-- Number Type ---------------------------------------------------------------<

-- NaN Check
isNaN' :: RealFloat a => a -> Bool
isNaN' a = a /= a

-- Sign Check
signum' :: (Ord a, Num a) => a -> Int
signum' x
        | x > 0 = 1
        | x < 0 = (-1)
        | otherwise = 0

-- Number Transformation -----------------------------------------------------<
