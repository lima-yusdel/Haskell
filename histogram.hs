--Helper method for skips
nth :: Int -> [a] -> [a]
nth n lst = [lst !! i | i <- [n-1, n-1+n..length lst -1]]

--skips
skips :: [a] -> [[a]]
skips lst = [nth i lst | i <- [1..length lst]]

--localMaxima
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []

--Helper methods
numCount :: [Integer] -> [Int]
numCount xs = map (\n -> length $ filter (== n) xs) [0..9]

marker :: [Int] -> Int -> String
marker xs n = [if x >= n then '*' else ' ' | x <- xs]

histogram :: [Integer] -> String
histogram xs = unlines (map (marker x) [n+1,n..1]) ++ "==========\n0123456789\n"
  where x = numCount xs
        n = maximum x
