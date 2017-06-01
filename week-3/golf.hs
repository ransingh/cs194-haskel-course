module Golf where

import Data.List

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:as) = Just a

skipByNAndPickHead :: Int -> [a] -> Maybe a
skipByNAndPickHead n =  safeHead . drop(n-1) . take n

skipAndPick :: Int -> [a] -> [Maybe a]
skipAndPick n [] = [Nothing]
skipAndPick n xs = skipByNAndPickHead n xs : skipAndPick n (drop n xs)

normalize :: [Maybe a] -> [a]
normalize (Nothing:xs) = []
normalize ((Just a):xs) = a : normalize xs

skipAndPickAndNormalize :: Int -> [a] -> [a]
skipAndPickAndNormalize n = (normalize . skipAndPick n)
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
skips :: [a] -> [[a]]
skips xs = [skipAndPickAndNormalize n xs | n <- [1..length(xs)]]

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:zs)
  | maximum x y z = y:localMaxima(y:z:zs)
  | otherwise = localMaxima $ y:z:zs
  where maximum x y z = if y > x && y > z then True else False


presenceMarker :: Maybe a -> Char
presenceMarker (Just a) = '*'
presenceMarker Nothing = ' '

checkPresence :: [Integer] -> [Char]
checkPresence xs = [presenceMarker $ elemIndex n xs | n <- [0..9]]

deleteAll :: [Integer] -> [Integer]
deleteAll xs = map delete [0..9]
--histogram :: [Integer] -> String
