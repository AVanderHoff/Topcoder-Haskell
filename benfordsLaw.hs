
benfordLaw xs t =  compare q threshold 1
   where y = fromIntegral ( length xs)
         z = map benfordLog [1.0 .. 9.0]
         q = map (*y) z
         w = map head $ map numtolist xs
         occurences m ns = fromIntegral (length $ filter (\q -> q==m) ns)
         appears = map (\r -> (occurences r w)) [1 .. 9]
         threshold = zip (map (/t) appears) (map (*t) appears)
         compare [] _ _ = -1
         compare (t:ts) ((a,b):as) n
             | (t>=a)&&(t<=b) = compare ts as (n+1)
             | otherwise = n

benfordLog d = logBase 10 (1 + (1/d))

numtolist 0 = []
numtolist x = (numtolist (x `div` 10)) ++ [x `mod` 10]


-- TopCoder problem "BenfordsLaw" used in SRM 155 (Division II Level Two)

-- Problem Statement
        
-- Pick a random financial transaction from the ledger of a typical business and there is about a 30% chance that the first non-zero digit of the amount of money involved is a 1. This counter-intuitive fact is a result of Benford's Law, discovered by astronomer Simon Newcomb in the late 1800's and rediscovered by physicist Frank Benford in the early 1900's. They found that real world data are not evenly distributed. Instead, given a random number related to some natural or social phenomenon satisfying certain conditions, the probability that the first non-zero digit of the number is D is log10(1 + 1/D).
-- Increasingly, financial auditors are using Benford's Law to detect possible fraud. Given a record of the financial transactions of a company, if some leading digit appears with a frequency that significantly differs from that predicted by Benford's Law, it is a signal that those transactions deserve a closer look.
-- Create a class BenfordsLaw with a method questionableDigit that takes a int[] transactions and an int threshold and returns the smallest digit that appears as the first non-zero digit of numbers in transactions with a frequency that is at least threshold times more or less than its expected frequency (e.g., more than three times the expected frequency or less than one-third the expected frequency when threshold=3). If no such digit exists, return -1.
-- For example, consider the data
--     5236    7290     200    1907    3336
--     9182      17    4209    8746    7932
--     6375     909    2189    3977    2389
--     2500    1239    3448    6380    4812
-- The following chart gives the actual frequency of each leading digit, and its expected frequency according to Benford's law.
--     digit    | 1    2    3    4    5    6    7    8    9
--     ---------|---------------------------------------------
--     actual   | 3    4    3    2    1    2    2    1    2
--     expected | 6.02 3.52 2.50 1.94 1.58 1.34 1.16 1.02 0.92
-- Assuming a threshold of 2, there are two digits that are questionable: the leading digit 1 appears less than half as often as expected and the leading digit 9 appears more than twice as often as expected. Since 1 is smaller than 9, the answer is 1.
-- Note that, although the expected frequencies have been rounded to two decimal places in the above table for the purposes of presentation, you should perform all such calculations without rounding. Errors of up to 0.000001 in the expected frequencies will not affect the final answer.
 
-- Definition
        
-- Class:  BenfordsLaw
-- Method: questionableDigit
-- Parameters: int[], int
-- Returns:    int
-- Method signature:   int questionableDigit(int[] transactions, int threshold)
-- (be sure your method is public)
    
 
-- Notes
-- -   In the formula for Benford's Law, log10 means the base-10 logarithm.
 
-- Constraints
-- -   transactions contains between 1 and 50 elements, inclusive.
-- -   Each element of transactions is between 1 and 999999999, inclusive.
-- -   threshold is between 2 and 10, inclusive.
-- -   Errors of up to 0.000001 in calculating the expected frequencies will not affect the final answer.
 
-- Examples
-- 0)  
        
-- { 5236,7290,200,1907,3336,9182,17,4209,8746,7932,
--   6375,909,2189,3977,2389,2500,1239,3448,6380,4812 }
-- 2
-- Returns: 1
-- The example above.
-- 1)  
        
-- { 1,10,100,2,20,200,2000,3,30,300 }
-- 2
-- Returns: 2
-- 2 and 3 appear more than twice as often as expected, and 4 through 9 appear less than half as often as expected. 2 is the smallest of these digits.
-- 2)  
        
-- { 9,87,765,6543,54321,43219,321987,21987,1987,345,234,123 }
-- 2
-- Returns: -1
-- 3)  
        
-- { 1,2,3,4,5,6,7,8,7,6,5,4,3,2,1 }
-- 8
-- Returns: 9
-- 4)  
        
-- { 987,234,1234,234873487,876,234562,17,
--   7575734,5555,4210,678234,3999,8123 }
-- 3
-- Returns: 8