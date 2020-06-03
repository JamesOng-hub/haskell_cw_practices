import Test.QuickCheck 

--write properties that will allow you to use quickcheck to test certian funcitons 
halfEvens :: [Int] -> [Int] 
halfEvens [] = [] 
halfEvens (h:t) = if mod h 2 == 0 then div h 2 : halfEvens t 
	                              else h : halfEvens t 

halfEvens' :: [Int] -> [Int] 
halfEvens' xs = map (\x -> if x `mod` 2 == 0 then x `div` 2 else x) xs 

prop_halfEvens :: [Int] -> Bool 
prop_halfEvens xs = halfEvens xs == halfEvens' xs 

inRanges :: Int -> Int -> [Int] -> [Int] 
inRanges min max xs = [ x | x <- xs, x>= min, x <= max] 

inRanges' :: Int -> Int -> [Int] -> [Int] 
inRanges' min max xs = filter (\x -> (x>=min) && (x<=max)) xs 

prop_inRanges :: Int -> Int -> [Int] -> Bool
prop_inRanges min max xs = inRanges min max xs == inRanges' min max xs 


prop_inRanges' :: Int -> Int -> [Int] -> Bool 
prop_inRanges' min max xs = processed >= take len (cycle [min]) && processed <= take len (cycle [max]) 
                           where processed = inRanges min max xs 
                                 len = length processed

countPositives :: [Int] -> Int 
countPositives [] = 0 
countPositives (h:t) = if (h>0) then 1 + countPositives t 
                                else countPositives t 

countPositives' :: [Int] -> Int 
countPositives' xs = length (filter (\x -> x >0) xs )    

prop_countPositives :: [Int] -> Bool 
prop_countPositives xs= countPositives xs == countPositives' xs 

                           