import Data.List (foldr) 

mult :: (Num a) => [a] -> a --type constrain saying 'a' must be a num typeclass 

mult xs = foldr (*) 1 xs 
--mult (x:xs) = foldl (\acc y  -> acc * y) x xs

--return only the positive integers in the list 
--take in a list 
--return another list
--they want you to use higher order functions 
posList :: (Ord a,Num a) => [a] -> [a] --why is Ord a so important? 
posList xs = filter (>0) xs 
--interesting, that's how you write predicates.

--determines if all booleans in the list are true
--returns true or false 
trueList ::  [Bool] -> Bool 
trueList xs = length (filter (==True) xs) == length xs 
--(filter (==True) xs) is going to return a list of all the TRUEs
--if you compare the list of those two, you can tell if they are all true. 

evenList :: [Int] -> Bool -- why can't i put typeclass Num a instead. 
evenList xs = length (filter even xs) == length xs 


--polymorphic function, that takes the maximum item of the list 
maxList :: (Ord a) => [a] -> a 
maxList  = foldr1 (\x y -> if x >= y then x else y) 

--specify a range and the function will print out all the numbers in
--that range, including those numbers . 
inRange :: Int -> Int -> [Int] -> [Int] 
inRange x y xs = filter (\n -> n>=x && n<=y) xs

--count the number of positive integers in a list
countPositives :: (Ord a,Num a) =>[a] -> Int  --countPositives and posList doesnt work. 
countPositives xs = length [posList xs]

-- write your own "length" function 
mylength :: [a] -> Int 
mylength xs = foldr (+) 0 (map (\x -> 1) xs)

mymap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) []

mylength' :: [a] -> Int 
mylength' = foldr ((+).(\x -> 1)) 0 xs 
--composite funciton 
-- apply the second funciton to the list then the first function. 

