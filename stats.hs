module Stats where

--factorial
fact :: Int -> Int
fact n = product [1..n]

--binomial coefficient function
choose :: Int -> Int -> Int
choose n k = (fact n) `div` ((fact k) * (fact (n-k)))

--pdf for binomial distribution
binomial :: Double -> Int -> Int -> Double
binomial p k n = (fromIntegral (choose n k)) * (p^n) * (1-p)^(n-k)

--pdf for poisson distribution
poisson :: Double -> Int -> Double
poisson l n = exp(-l) * (l^n) / fromIntegral (fact n)

--pdf for uniform distribution
uniform :: Double -> Double -> Double -> Double
uniform a b _ = 1/(b-a)

--cdf takes a pdf and a bound, then returns the cumulative probability
--from 0 to that bound.
cdf :: (Enum a, Num a, Num b) => (a -> b) -> a -> b
cdf f n = sum $ map f [0..n]

--cdf' is like cdf, but takes a precision for approximating an integral
--can only use 1 type
cdf' :: (Enum a, Num a) => (a -> a) -> a -> a -> a
cdf' f n p = sum $ map (*p) $ map f [0,p..n]
