import Data.Char (toUpper)
import Data.List (zip, drop, take)

--count positive numbers in a list 
countPositives :: [Int] -> Int 
countPositives [] = 0 
countPositives (x:xs) = if x > 0
	                    then 1 + countPositives xs 
	                        else countPositives xs

--insertion sort
--empty list is already sorted, 
--non-empty list, sorting the tail and then inserting the head into the result. 
isort :: Ord a => [a] -> [a] --Ord a is a typeclass contraint 
isort [] = [] 
isort (x:xs) = insert x (isort xs)

insert :: (Ord a) => a -> [a] -> [a] 
insert x [] = [x] --base case 
insert x (y:ys) = if x <= y    --(y:ys) is used to represent a list. weird....
                  then x:(y:ys)
                  else y: insert x ys --draw out a visualisation 

--merge two sorted list to give a single sorted list. 
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs 
merge [] ys = ys 
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)  --these are called guarded equations 
                    | otherwise = y : merge (x:xs) ys


halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = (take half xs, drop half xs)
           where half = length xs `div` 2 

msort :: (Ord a) => [a] -> [a] 
msort [x] = [x] 
msort xs = merge (msort left) (msort right) 
           where (left, right) = halve xs 

rotor :: Int -> String -> String  
rotor n [] = error "Empty list" 
rotor n xs | n > length xs = error "greater than the lenght of the list"  --these are called guarded equations. 
           | n < 0 = error "Negative number can't be operated on" 
           | n == 0 = xs 
           | otherwise = drop n xs ++ take n xs    -- drop the first n characters and then add it to the end of that. 

-- rotor n (c:str) | (0 <= n) && (n < length (c:str)) = rotor (n-1) (str ++ [c])

--takes in a number called offset, gives a list of pairs , in which (alphabet, offset alphabet)
--assume that all keys are given in terms of Upper keys only.
makeKey :: Int -> [(Char, Char)]
makeKey n = zip alphabet (rotor n alphabet)
            where alphabet = ['A' .. 'Z']

--takes in the first elem of a pair and returns the second elem 
--from a list of pairs 
lookUp :: Char -> [(Char, Char)] -> Char 
lookUp char [] = char 
lookUp char ((a,key): aks) = if char == a then key  --(x:xs) somehow, haskell understand that you are refering the rest of the list by saying this. 
                             else lookUp char aks  

--encrypts a single upper case letter using a single offset 
--takes in an offset, and a letter 
encipher :: Int -> Char -> Char 
encipher n char = lookUp char dict 
                  where dict = makeKey n 

--whole string set to upper case 
--remove all characters except for letters and digits 
normalise :: String -> String --string is a list of characters 
--for uppercase letters and digits, take them and add to the list 
--for lowercase letters, turn them to uppercase and then add them into the list 
--for anything else, dont add anything to the list/ return the original list. 
normalise [] = [] 
normalise (x:xs) |(x `elem` ['A' .. 'Z']) || (x `elem` ['0' .. '9']) =  x: normalise xs
                 | x `elem` ['a'..'z'] = (toUpper x) : normalise xs 
                 | otherwise = normalise xs 
--non exhaustive meaning that when presented with lower case, it func doesnt know what to do. 

--funciton normalise the string and then encipher it . 
encipherStr :: Int -> String -> String 
encipherStr n [] = [] 
encipherStr 0 xs = xs 
encipherStr n xs = [encipher n x | x <- list1 ] where list1 = normalise xs 


decipherStr :: String -> [String]d 
decipherStr str = deciperStrHelper 0 str

deciperStrHelper :: Int -> String -> [String] 
deciperStrHelper 26 str = [] 
deciperStrHelper n str = (encipherStr n str) : deciperStrHelper (n+1) str  -- will give a list of possible deciphered strings. 









