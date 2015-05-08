
module Golf where


skip :: Int -> [a] -> [a]
skip n xs = (map snd . filter (\x -> mod (fst x) n == 0) . zip [1..]) xs

skips :: [a] -> [[a]]
skips xs = zipWith ($) (map skip [1..]) (replicate (length xs) xs)

noBeg :: [(Integer, Integer)] -> [(Integer, Integer)]
noBeg xs = filter (\x -> fst x /= (minimum (map fst xs))) xs

noEnd :: [(Integer, Integer)] -> [(Integer, Integer)]
noEnd xs = filter (\x -> fst x /= (maximum (map fst xs))) xs

noBegEnd :: [(Integer, Integer)] -> [(Integer, Integer)]
noBegEnd xs = noBeg . noEnd $ xs

rightList :: [a] -> [[a]]
rightList (x:y:z:rest) = [x,y,z] : rightList (y:z:rest)
rightList _ = []

compareD :: [Integer] -> [Integer]
compareD (x:y:z)
  | maximum (x:y:z) == y = [y]
  | otherwise = []


localMaxima :: [Integer] -> [Integer]
localMaxima = reduceD . filter (not . null) . map compareD . rightList

reduceD :: [[Integer]] -> [Integer]
reduceD (x:xs) = x ++ (reduceD xs)
reduceD _ = []
