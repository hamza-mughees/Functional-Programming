{- butrfeld Andrew Butterfield -}
module Ex04 where

name, idno, username :: String
name      =  "Hamza Mughees"  -- replace with your name
idno      =  "17329860"    -- replace with your student id
username  =  "mugheesh"   -- replace with your TCD username

declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype, honestly!
data BinTree k d
  = Branch (BinTree k d) (BinTree k d) k d
  | Leaf k d
  | Empty
  deriving (Eq, Show)


-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> BinTree k d -> BinTree k d
ins k d (Branch left right k' d')
  | k < k' = Branch (ins k d left) right k' d'
  | k > k' = Branch left (ins k d right) k' d'
  | otherwise = Branch left right k d
ins k d (Leaf k' d')
  | k < k' = Branch (Leaf k d) (Empty) k' d'
  | k > k' = Branch (Empty) (Leaf k d) k' d'
  | otherwise = Leaf k d
ins k d Empty = Leaf k d

-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => BinTree k d -> k -> m d
lkp (Branch left right k' d') k = do
  if k < k' then do
    x <- lkp left k
    return x
  else if k > k' then do
    x <- lkp right k
    return x
  else do
    return d'
lkp (Leaf k' d') k = do
  if k == k' then do
    return d'
  else do
    fail "FAIL"
lkp Empty _ = do
  fail "FAIL"

-- Part 3 : Tail-Recursive Statistics

{-
   It is possible to compute BOTH average and standard deviation
   in one pass along a list of data items by summing both the data
   and the square of the data.
-}
twobirdsonestone :: Double -> Double -> Int -> (Double, Double)
twobirdsonestone listsum sumofsquares len
 = (average,sqrt variance)
 where
   nd = fromInteger $ toInteger len
   average = listsum / nd
   variance = sumofsquares / nd - average * average

{-
  The following function takes a list of numbers  (Double)
  and returns a triple containing
   the length of the list (Int)
   the sum of the numbers (Double)
   the sum of the squares of the numbers (Double)

   You will need to update the definitions of init1, init2 and init3 here.
-}
getLengthAndSums :: [Double] -> (Int,Double,Double)
getLengthAndSums ds = getLASs init1 init2 init3 ds
init1 = 0
init2 = 0
init3 = 0

{-
  Implement the following tail-recursive  helper function
-}
getLASs :: Int -> Double -> Double -> [Double] -> (Int,Double,Double)
getLASs i1 i2 i3 (x:xs) = getLASs (i1+1) (i2+x) (i3+(x*x)) xs
getLASs i1 i2 i3 [] = (i1, i2, i3)

-- Final Hint: how would you use a while loop to do this?
--   (assuming that the [Double] was an array of double)
