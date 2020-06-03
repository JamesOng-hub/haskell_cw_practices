
import Data.Char (toUpper, toLower)

--squares a number 
square :: Int -> Int 
square a = a*a 

--takes in two integers and returns the sum of the two integers 
{-pyth :: Int -> Int -> Int 
pyth a b = a*a + b*b -}
pyth :: Int -> Int -> Int 
pyth a b = square a + square b 

isTriple :: Int -> Int -> Int -> Bool {-
isTriple a b c = if a*a + b*b == c*c then True else False -}
isTriple a b c = pyth a b == square c 

isTripleAny :: Int -> Int -> Int -> Bool 
isTripleAny a b c = isTriple a b c || isTriple a c b || isTriple c b a 

--div by 2 all the even numbers 
halfEven :: [Int] -> [Int]
halfEven xs = [if even x then div x 2 else x | x <- xs ] -- after | are conditions 

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b [] = [] 
inRange a b xs = [x | x <- xs, x>=a, x<=b ]

countPositives :: [Integer] -> Int 
countPositives [] = 0
countPositives xs = sum[1 | x <- xs, x > 0] --get the sum of the list. (the condition says that we take elements from xs and if it is larger than 0 , than put 1 in the list. )



capitalised :: String -> String --capitalised [] = []   --you introduced y, so you need to define what is y. 
capitalised xs = [toUpper y | y <- xs, y == head xs] ++ [toLower z | z <- xs, z /= head xs]
--capitalised (x:xs) = [toUpper x] ++ [toLower xs] --toUpper, toLower converts one letter at a time, that's why this doesnt work. 

--write a function toLower every character in a word 
lowerCase :: String -> String 
lowerCase xs = [toLower x | x <- xs ]
--lowerCase word = [toLower w | w<-word ]


--title function, takes in a list, capitalised the first word, and any other word that is longer than 4 characters
title :: [String] -> [String]
title xs = [if length x > 3 || x == head xs 
            then capitalised x 
            else lowerCase x | x <- xs]

