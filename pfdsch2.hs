-- Excercises for Purely Functional Data Structures chapter 2

data (Ord a) =>
    UnbalancedSet a = E
                    | T (UnbalancedSet a) a (UnbalancedSet a)

empty :: UnbalancedSet a
empty = E

member :: (Ord a) => UnbalancedSet a -> a -> Bool
member = member_ Nothing

-- Solution to excercise 2.2.
-- It guarantees at most D+1 element comparisons, where D is the depth of
-- the tree representation of the given set.
-- This is guaranteed by always going down either left or right after one
-- < comparison per node (max D comparisons), and keeping track of a candidate
-- node c meaning "the closest matching node we've passed so far".
-- When going down left, the current node y CANNOT be x, so pass c unchanged.
-- When going down right, the current node y MAY be x, so pass y as new c.
-- When we reach the bottom, return whether c matches x. (1 comparison)
member_ :: (Ord a) => Maybe a -> UnbalancedSet a -> a -> Bool
member_ c t x = case t of
    E       -> case c of
      Nothing -> False
      Just y  -> x == y
    T l y r -> if x < y
               then member_ c l x
               else member_ (Just y) r x

------------
-- Mundanity

-- set of 1, 3, 4, 5, 6, 9.
testSet :: (Ord a) => (Num a) => UnbalancedSet a
testSet = T (T (T E 1 E) 3 (T E 4 E)) 5 (T (T E 6 E) 9 E)

main = do
    let t = testSet
    putStrLn $ show $ member t 0
    putStrLn $ show $ member t 1
    putStrLn $ show $ member t 4
    putStrLn $ show $ member t 5
    putStrLn $ show $ member t 6
    putStrLn $ show $ member t 7
