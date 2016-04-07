module Utils where

apnd :: [a] -> a -> [a]
apnd as a = as ++ [a]

get :: [a] -> Int -> Maybe a
get [] _ = Nothing
get (x:_) 0 = Just x
get (x:xs) i = get xs $ i - 1
