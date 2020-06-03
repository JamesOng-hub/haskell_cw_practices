module Coursework where

{-
  Your task is to design a datatype that represents the mathematical concept of a (finite) set of elements (of the same type).
  We have provided you with an interface (do not change this!) but you will need to design the datatype and also 
  support the required functions over sets.
  Any functions you write should maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a list
  (as in the example below). Alternatively, one could use an algebraic data type,
  wrap a binary search tree, or even use a self-balancing binary search tree.
  Extra marks will be awarded for efficient implementations (a self-balancing tree will be
  more efficient than a linked list for example).

  You are NOT allowed to import anything from the standard library or other libraries.
  Your edit of this file should be completely self-contained.

  DO NOT change the type signatures of the functions below: if you do,
  we will not be able to test them and you will get 0% for that part. While sets are unordered collections,
  we have included the Ord constraint on most signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Please include everything in this file.
-}



--for the forList func 
qsort ::(Ord a) => [a] -> [a] 
qsort [] = [] 
qsort (x:xs) = qsort [ a | a <- xs, a <= x ] ++ [x] ++ qsort [ b | b <- xs , b > x ]

--for the insert func 
insertItem :: (Ord a) => a -> [a] -> [a] 
insertItem x [] = [x] 
insertItem y (x:xs) = if y == x then (x:xs) 
                      else if y <= x then y:(x:xs) else x: insertItem y xs 

--for the union func 
merge ::(Ord a) =>  [a] -> [a] -> [a] 
merge [] [] = [] 
merge (xs) [] = xs 
merge [] (ys) = ys 
merge (x:xs) (y:ys) = if y == x then merge xs (y:ys) 
                                else if  x < y then x: merge (xs) (y:ys) 
                                               else y: merge (x:xs) (ys) 

--for the intersection, difference and member func
compare' :: (Ord a) => a -> [a] -> [a] 
compare' a [] = [] 
compare' a (x:xs) = if a == x then a : compare' a (xs) else compare' a (xs) 

member' :: (Ord a) => [a] -> Bool 
member' xs = if length xs > 0 then True else False 

--for the powerSet func 
insert' :: a -> [a] -> [a] 
insert' x [] = [x] 
insert' y (x:xs) = y:(x:xs) 

insertSet :: a -> Set a -> Set a 
insertSet x (Set a) = Set (insert' x a) 

powerSet' :: Set a  -> [Set a] 
powerSet' (Set [])  = [Set []] 
powerSet' (Set (x:xs)) = powerSet' (Set xs) ++ map (insertSet x) (powerSet' (Set xs)) 



-- Error non-exhaustive pattern means insufficient pattern matching
 

{-data Tree a = Tip | Node a (Tree a) (Tree a)
   PART 1.
   You need to define a Set datatype. Below is an example which uses lists internally.
   It is here as a guide, but also to stop ghci complaining when you load the file.
   Free free to change it.
-}

-- you may change this to your own data type
data Set a = Set { unset :: [a] } deriving (Show) 



 
{-
   PART 2.
   If you do nothing else, at least get the following two functions working. They
   are required for testing purposes.
-}

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: (Ord a) => Set a -> [a]
toList set = qsort (unset set)  

-- fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: Ord a => [a] -> Set a
fromList xs = Set (qsort  xs)
-- fromList xs = Set (qsort (unset (Set xs)))

{-
   PART 3.
   Your Set should contain the following functions.
   DO NOT CHANGE THE TYPE SIGNATURES.
-}

-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where --instance typeConstrain => class(type) where 
  s1 == s2 = if unset s1 == unset s2 then True else False 





-- the empty set
empty :: Set a
empty = Set [] 


-- Set with one element
singleton :: a -> Set a
singleton a = Set (a:[]) 


-- insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert a (Set xs) = Set(insertItem a xs)




-- join two Sets together
-- be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union (Set (x:xs)) (Set (y:ys)) = Set (merge (x:xs) (y:ys))  



-- return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = Set (foldr (\ x acc -> if member'(compare' x ys) then x:acc else acc)[] xs)

-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference (Set xs) (Set ys) = Set (foldr (\ x acc -> if member'(compare' x ys) then acc else x:acc) [] xs)


-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member a (Set (x:xs)) = member' (compare' a (x:xs) )


-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality (Set xs) = foldl (+) 0 (map (\x->1) xs)  


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f (Set xs)= Set(map f (xs))  


setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f (Set (xs)) init = foldr f init xs


-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)
powerSet set = Set(powerSet' set) 

-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian (Set xs) (Set ys) = Set [ (x,y) | x <- xs , y <-ys ]


-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (a -> Bool) -> Set a -> (Set a, Set a) --takes in a predicate, a set - then gives two sets 
partition pred (Set xs) = (Set (filter pred xs),  Set(filter (not.pred) xs)) 

         

--f(Set (filter pred xs),  Set(filter not(pred) xs)) 


{-
   On Marking:
   Be careful! This coursework will be marked using QuickCheck, against Haskell's own
   Data.Set implementation. Each function will be tested for multiple properties.
   Even one failing test means 0 marks for that function.

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough for a
   passing mark of 40%.

   The maximum mark for those who use lists, as in the example above, is 70%. To achieve
   a higher grade than is, one must write a more efficient implementation.
   100% is reserved for those brave few who write their own self-balancing binary tree.
-}
