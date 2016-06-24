module Utils where
import qualified Data.List as L

eq :: Eq a => a -> a -> Bool
eq = (==)

apnd :: [a] -> a -> [a]
apnd as a = as ++ [a]

arvs :: a -> b -> (b, a)
arvs a b = (b, a)

tfst :: (a, b, c) -> a
tfst (a, b, c) = a

sndthd :: (a, b, c) -> (b, c)
sndthd (a, b, c) = (b, c)

tthd :: (a, b, c) -> c
tthd (a, b, c) = c

get :: [a] -> Int -> Maybe a
get [] _ = Nothing
get (x:_) 0 = Just x
get (x:xs) i = get xs $ i - 1

charToBit :: Char -> Int
charToBit '1' = 1
charToBit '0' = 0
charToBit _ = error "The character you're trying to turn into a bit isn't a 1 || 0. Something weird happened."

xor :: Char -> Char -> Int
xor x y = abs $ charToBit x - charToBit y

bitsToDec :: [Int] -> Int
bitsToDec = L.foldl' (\acc x -> acc * 2 + x) 0

zeroPad :: Int -> String -> String
zeroPad i s = pad ++ s
   where pad = replicate (i - length s) '0'

decToBitString :: Int -> String
decToBitString 0 = ""
decToBitString x = decToBitString next ++ (show remainder)
  where remainder = mod x 2
        next = div x 2
