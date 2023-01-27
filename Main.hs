bisect :: (Double -> Double) -> (Double, Double) -> Double -> Double
bisect f (a,b) epsilon
    | abs (b - a) < epsilon = (a + b) / 2
    | f ((a + b) / 2) == 0 = (a + b) / 2
    | f (a) * f ((a + b) / 2) < 0 = bisect f (a, (a + b) / 2) epsilon
    | otherwise = bisect f ((a + b) / 2, b) epsilon

f :: Double -> Double
f x = x^3 - x - 2

main :: IO ()
main = do
    print $ bisect f (1, 2) 1e-6
    -- Output: 1.5213799476623535