fraccount w z = fraccountH w z 1 2 1
    where  fraccountH x y a b count
  		| (x==a) && (y==b) = count
  		| (a+1)==b = fraccountH x y 1 (b+1) (count+1)
  		| a==1 = fraccountH x y (a+1) b (count+1)
  		| (a `gcd` b)==1 = fraccountH x y (a+1) b count+1
  		| otherwise = fraccountH x y (a+1) b count




--   		TopCoder problem "FracCount" used in SRM 247 (Division II Level Two)

-- Problem Statement
--     	It is possible to assign a unique integer value to each irreducible fraction between 0 and 1. (This shows that there are a countable infinity of fractions.) The usual way to number them is shown below

-- 1/2  1/3  2/3  1/4  3/4  1/5  2/5  3/5  4/5  1/6  5/6  1/7  ...
 
-- Notice that 2/4, for example, does not get listed because it reduces to 1/2. Given an irreducible fraction we want to find where it appears in the above counting order, where 1/2 is counted as 1, 1/3 as 2, etc.
-- Create a class FracCount that contains a method position that is given the numerator and denominator of an irreducible fraction between 0 and 1 and that returns its position in the counting order.
 
-- Definition
    	
-- Class:	FracCount
-- Method:	position
-- Parameters:	int, int
-- Returns:	int
-- Method signature:	int position(int numerator, int denominator)
-- (be sure your method is public)
    
 
-- Constraints
-- -	numerator will be between 1 and denominator - 1 inclusive.
-- -	denominator will be between 2 and 1,000 inclusive.
-- -	The greatest common divisor of numerator and denominator will be 1.
 
-- Examples
-- 0)	
    	
-- 1
-- 2
-- Returns: 1
-- 1/2 is at position 1 in the counting order
-- 1)	
    	
-- 5
-- 6
-- Returns: 11
-- 5/6 is at position 11 in the counting order
-- 2)	
    	
-- 999
-- 1000
-- Returns: 304191
