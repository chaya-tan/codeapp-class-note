factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

map' :: (a -> b) -> [a] -> [b]
map' function [] = []
map' function [a] = [function a]
map' function (x:xs) = function x : map' function xs

add2Tuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2Tuples a b = (fst a + fst b, snd a + snd b) 

countElm :: [a] -> String
countElm [] = "empty"
countElm (_:[]) = "one"
countElm (_:_:[]) = "two"
countElm _ = "many"

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' _ [] = []
zip' [] _ = []
zip' (a:as) (b:bs) = [(a,b)] ++ zip' as bs

-- unzip' :: [(a, b)] -> ([a], [b])
-- unzip' [] = ([],[])

prime_to :: Int -> [Int]
prime_to n = sieve [2..n]
    where   sieve [] = []
            sieve (x:xs) = x : sieve [y | y <- xs, (y `rem` x) /= 0] -- `rem` คือหาเศษ ก็คือเอาเศษไม่เท่ากับ 0

-- Permute
-- เอาของในลิสมาเรียงให้ครบทุกแบบ โดยที่ทุกตัวที่ใส่มาเป็น unique
-- เช่น [a,b,c] -> [a,b,c] [a,c,b] ... [c,b,a]
-- จะเห็นว่า [a,b,c] [a,c,b] ไอตัวด้านหลัง b,c กับ  c,b ก็เป็น permute ย่อย ๆ ในนั้น -> เขียนเป็น recursion ได้
-- แต่จะมีพวก [b,a,c] [b,c,a] ฉะนั้นดึงตัวแรกออกมาแล้วต่อกันตรง ๆ ไม่ได้ ต้องใช้ list comprehension ช่วย
permute :: (Eq a) => [a] -> [[a]]
permute [] = [[]]
permute xs = [x:ys | x <- xs, ys <- permute (delete x xs)]

-- หอคอยฮานอย https://en.wikipedia.org/wiki/Tower_of_Hanoi
-- ให้หา step ว่าต้องเอาแท่งไหนไปแท่งไหนถึงจะได้ มี 3 เสาเสมอ
-- ให้ n เป็นจำนวนแผ่น
hanoi :: Int -> [(Int,Int)]
hanoi n = hanoi' n 1 2 3
hanoi' 0 _ _ _ = []
้hanoi' n beg tmp dist = tops_to_tmp ++ bottom_to_dist : top_to_dest
    where   tops_to_tmp = hanoi' (n-1) beg dest tmp
            bottom_to_dist = (beg, dest)
            top_to_dest = (hanoi' (n-1) tmp beg dest)
