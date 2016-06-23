turtleDove x = turtleDove2 (turtleDove1 x 1) 2 

turtleDove1 n d 
   | n <= 0 = n
   | otherwise =  turtleDove1 (n - (((d^2)+d) `div` 2)) (d+1) 

turtleDove2 n t
    | n>=0 = t-1
    | otherwise = turtleDove2 (n + t)  (t+1)   
              



-- TopCoder problem "TwoTurtledoves" used in SRM 224 (Division I Level One , Division II Level Two)

-- Problem Statement
    	
-- I give my true love presents every day. On day 1, I give her one Type 1 present. On day 2, I give her two Type 2 presents, followed by one Type 1 present. On day 3, I give her three Type 3 presents, followed by two Type 2 presents, followed by one Type 1 present. This pattern continues day after day. Soon I begin to wonder, what type of present will be the 100th present that I give my true love? The 1000th? The 1000000th?
-- For example, the 10th present that I give my true love is the Type 1 present on day 3.
-- Write a method that takes an int n and returns the type of the nth present that I give my true love. Note that n is one-based, so n=1 refers to the very first present I give her (the Type 1 present on day 1).
 
-- Definition
    	
-- Class:	TwoTurtledoves
-- Method:	presentType
-- Parameters:	int
-- Returns:	int
-- Method signature:	int presentType(int n)
-- (be sure your method is public)
    
 
-- Notes
-- -	Unlike the poseur in the popular Christmas song, I do not stop giving my true love presents after the 12th day.
 
-- Constraints
-- -	n is between 1 and 1000000000, inclusive.
 
-- Examples
-- 0)	
    	
-- 10
-- Returns: 1
-- The example above.
-- 1)	
    	
-- 12
-- Returns: 4
-- The second of four Type 4 presents on Day 4.
-- 2)	
    	
-- 399
-- Returns: 11
-- 3)	
    	
-- 123456
-- Returns: 65
-- 4)	
    	
-- 1000000000
-- Returns: 1704
