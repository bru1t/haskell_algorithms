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

-- Solving a quadratic equation in format: (a,b,c) -> a*x^2+b*x+c=0
quadeq :: (Float, Float, Float) -> [Float]
quadeq (0,b,c) = [ (-c) / b]
quadeq (a,b,c)
        | d == 0 = [ root (+) ]
        | otherwise = [ root (+) , root (-) ]
               where
                       d = b*b-4*a*c
                       root sign = sign (-b) (sqrt d) / (2*a)
