import Data.List

snowFlakes xs  = (reverse addRev) ++ addRev  
   where flipped = transpose xs
         dropped = map (drop 1) flipped
         diag = map (\z -> (fst z) ++ (snd z)) (zip xs dropped)
         addRev = map (\q -> (reverse q) ++ q) diag



-- TopCoder problem "Snowflakes" used in SRM 291 (Division I Level One , Division II Level Two)

-- Problem Statement
    	
-- Paper snowflakes make great decorations and they are easy to make. Take a sheet of square checkered paper with an even number of squares on each side. Fold it in half, folding the upper part up and over to the bottom part, so that you have a rectangle. Now fold that rectangle in half lengthwise, from left to right, to make a smaller square. Next, take the right upper corner of that square and fold it up and over to the left bottom corner, forming a triangle (see pictures below, cells which stay immovable after all foldings are marked red on the fifth picture). Cut out some squares along grid lines (as shown on the last picture).
-- ? ? ? ? ? 
-- You will be given a String[] snowflake where each element represents a single row of squares in the folded pattern (the last picture shown above). The elements in snowflake are ordered from top row to bottom row, and each row is ordered from left to right. '*' represents a cut out square, and '.' represents an intact square. Unfold the triangle by reversing the steps described above. Return this flared out snowflake as a String[] of '.' and '*' characters. You should return the contents of the entire square even if the cut out cells split the snowflake into non-connected parts.
 
-- Definition
    	
-- Class:	Snowflakes
-- Method:	flareOut
-- Parameters:	String[]
-- Returns:	String[]
-- Method signature:	String[] flareOut(String[] snowflake)
-- (be sure your method is public)
    
 
-- Constraints
-- -	snowflake will contain between 1 and 50 elements, inclusive.
-- -	The ith element of snowflake (0-indexed) will contain exactly i+1 characters.
-- -	Each element of snowflake will contain only characters '*' or '.'.
 
-- Examples
-- 0)	
    	
-- {".",
--  "..",
--  "*.*"}
-- Returns: {"*.**.*", "......", "*....*", "*....*", "......", "*.**.*" }
-- 1)	
    	
-- {"*",
--  "..",
--  ".*.",
--  ".**.",
--  ".*.**"}
-- Returns: 
-- {"**.*..*.**",
-- "*.**..**.*",
-- ".*.*..*.*.",
-- "***....***",
-- "....**....",
-- "....**....",
-- "***....***",
-- ".*.*..*.*.",
-- "*.**..**.*",
-- "**.*..*.**" }
-- 2)	
    	
-- {".",
--  "..",
--  "***"}
-- Returns: {"******", "*....*", "*....*", "*....*", "*....*", "******" }
-- 3)	
    	
-- {"*",
--  ".*",
--  "***"}
-- Returns: {"******", "**..**", "*.**.*", "*.**.*", "**..**", "******" }
-- 4)	
    	
-- {".",
--  "..",
--  "***",
--  "...."}
-- Returns: 
-- {"........",
-- ".******.",
-- ".*....*.",
-- ".*....*.",
-- ".*....*.",
-- ".*....*.",
-- ".******.",
-- "........" }
