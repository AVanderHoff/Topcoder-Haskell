
testCurve xs = finalScores
    where maxXS = maximum xs
          curveRatio = 100.00 / (fromIntegral maxXS)
	  xXS = map fromIntegral xs
          newScores = map (* curveRatio) xXS
          finalScores = map toInteger $ map floor newScores 


--  Problem Statement for TestCurve


-- Problem Statement
    	
-- A teacher has graded a test, and you are given a int[] scores representing each student's raw score on the test. The teacher wants to convert the scores into grades such that a raw score of 0 receives a grade of 0, and the highest raw score achieved by any student receives a grade of 100. All other scores in between are calculated proportionally, always rounded down. You are to return a int[] (with the same number of elements as scores) containing the calculated grade for each student. Element i of the return should be the grade for scores[i].

 
-- Definition
    	
-- Class:	TestCurve
-- Method:	determineGrades
-- Parameters:	int[]
-- Returns:	int[]
-- Method signature:	int[] determineGrades(int[] scores)
-- (be sure your method is public)
    
 
-- Constraints
-- -	scores will contain between 1 and 50 elements, inclusive.
-- -	Each element of scores will be between 0 and 10000, inclusive.
-- -	At least one element of scores will be greater than 0.
 
-- Examples
-- 0)	
    	
-- {15, 27, 8, 33, 19, 50}
-- Returns: {30, 54, 16, 66, 38, 100 }
-- Since the highest score is a 50, which gets scaled to a grade of 100, everyone's grade is exactly twice their raw score.
-- 1)	
    	
-- {0, 0, 0, 3}
-- Returns: {0, 0, 0, 100 }
-- All but one person did poorly on this test.
-- 2)	
    	
-- {67, 89, 72, 100, 95, 88}
-- Returns: {67, 89, 72, 100, 95, 88 }
-- With a highest score of 100, the grade is exactly the raw score.
-- 3)	
    	
-- {1234, 3483, 234, 5738, 3421, 5832, 4433}
-- Returns: {21, 59, 4, 98, 58, 100, 76 }
-- 4)	
    	
-- {8765}
-- Returns: {100 }
-- This problem statement is the exclusive and proprietary property of TopCoder, Inc. Any unauthorized use or reproduction of this information without the prior written consent of TopCoder, Inc. is strictly prohibited. (c)2010, TopCoder, Inc. All rights reserved.




-- This problem was used for: 
--        Single Round Match 276 Round 1 - Division II, Level One