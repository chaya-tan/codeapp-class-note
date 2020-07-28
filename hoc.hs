-- Higher order function

twice :: (a -> a) -> a -> a
twice function a = function (function a)

thrice :: (a -> a) -> a -> a
thrice function a = function (function (function a))
-- or
-- thrice function a = (function . function . function) a

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' function (x:xs) = function x : map' function xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' function (x:xs)
    | function x    = x : filter' function xs
    | otherwise     = filter' function xs