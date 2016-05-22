
rogersPhenomenon xs ys = filterXSYS + filterYSXS
    where avgXS =  (fromIntegral (sum xs)) / (fromIntegral (length xs))
          avgYS = (fromIntegral (sum ys)) / (fromIntegral (length ys))         
          filterXS = filter (< avgXS ) (map fromIntegral xs )
          filterYS = filter (< avgYS) (map fromIntegral ys)
          filterXSYS = length $ filter (> avgYS) filterXS
          filterYSXS = length $ filter (> avgXS) filterYS   


--  Problem Statement for RogersPhenomenon


-- Problem Statement
    	
-- The Will Rogers phenomenon is the apparent paradox which occurs when, upon moving an element from one set of numbers to another set, the means (averages) of both sets increase.

-- For example, the sets { 1, 2, 3 } and { 4, 5, 6 } have means 2 and 5, respectively. Moving the 4 to the first set would increase its mean to 2.5, while also increasing the mean of the second set to 5.5.

-- More precisely, we will consider the phenomenon to have occured if, upon moving a number between sets, the mean of both of the sets has strictly increased. An element may be moved from a set only if the set contains at least two elements.

-- You are to move exactly one number from one of the sets to the other. Given the initial configuration of the sets as two int[]s, set1 and set2, return the number of elements which trigger the phenomenon when chosen to be moved.

 
-- Definition
    	
-- Class:	RogersPhenomenon
-- Method:	countTriggers
-- Parameters:	int[], int[]
-- Returns:	int
-- Method signature:	int countTriggers(int[] set1, int[] set2)
-- (be sure your method is public)
    
 
-- Notes
-- -	The sets may contain duplicate elements, all of which should be included when calculating averages.
 
-- Constraints
-- -	Both set1 and set2 will contain between 1 and 50 elements, inclusive.
-- -	Each element in set1 and set2 will be between -1000 and 1000, inclusive.
 
-- Examples
-- 0)	
    	
-- { 1, 2, 3 }
-- { 4, 5, 6 }
-- Returns: 1
-- This is the example from the problem statement. The number 4 from the second set is the only one which triggers the Rogers phenomenon.
-- 1)	
    	
-- { 3, 100, 90 }
-- { 5, 1, 18, 29 }
-- Returns: 0
-- 2)	
    	
-- { 1, 1, 1, 1, 1, 5 }
-- { -10, -9, -8, -7, -6 }
-- Returns: 5
-- The sets may contain duplicate elements. Moving any of the 1's from the first set increases the means of both sets.
-- 3)	
    	
-- { 12, 94, 53, 43, 58, 85, 55, 83, 47, 57 }
-- { 23, 100, 98, 27, 9, 70, 39, 39, 54 }
-- Returns: 4
-- This problem statement is the exclusive and proprietary property of TopCoder, Inc. Any unauthorized use or reproduction of this information without the prior written consent of TopCoder, Inc. is strictly prohibited. (c)2010, TopCoder, Inc. All rights reserved.




-- This problem was used for: 
--        Single Round Match 277 Round 1 - Division I, Level One 
--        Single Round Match 277 Round 1 - Division II, Level Two