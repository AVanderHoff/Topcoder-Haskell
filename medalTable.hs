import Data.List

medalList xs = [a,b,c]
   where a = take 3 xs
         b = take 3 $ drop 4 xs
         c =  drop 8 xs

medalWinners xs = transpose $ map medalList xs
medalCount xs = nub $ concat $ map medalList xs

timesWon [] _ = []
timesWon (x:xs) (a:b:c) = (x , (countMedals x a), (countMedals x b) , (countMedals x (head c))) : timesWon xs (a:b:c)
   where countMedals y ys = length ( filter (\z -> (z==y)) ys)

medalSample = ["ITA JPN AUS", "KOR TPE UKR", "KOR KOR GBR", "KOR CHN TPE"]
medalSample2 = timesWon (medalCount medalSample) (medalWinners medalSample)

tuple1 (a,_,_,_) = a
tuple2 (_,b,_,_) = b
tuple3 (_,_,c,_) = c
tuple4 (_,_,_,d) = d



medalQuickSort  [] = []
medalQuickSort  (x:xs) = smaller ++ [x] ++ larger
   where smaller =  medalQuickSort  [y | y <- xs , medalSortLessThan y x ]
         larger = medalQuickSort [w | w <- xs , medalSortGreater w x  ] 


medalSortGreater (a,b,c,d) (e,f,g,h) 
   | b > f = True
   | (b==f)&&(c>g)= True
   | (b==f)&&(g==c)&&(d>h) = True
   | (b==f)&&(g==c)&&(h==d)&&(a<e) = True  
   | otherwise = False

medalSortLessThan (a,b,c,d) (e,f,g,h)
   | b < f = True
   | (b==f) && (g > c) = True
   | (b==f)&&(g==c)&&(h>d) = True
   | (b==f)&&(g==c)&&(h==d)&&(e < a) = True
   | otherwise = False

toStringQuadTuple (a,b,c,d) = a++" "++e++" "++f++" "++g
   where e = show b
         f = show c
         g = show d

medalTable xs = reverse $ map toStringQuadTuple ( medalQuickSort (timesWon (medalCount xs) (medalWinners xs)))



-- TopCoder problem "MedalTable" used in SRM 209 (Division I Level One , Division II Level Two)

-- Problem Statement
--       The Olympic Games in Athens end tomorrow. Given the results of the olympic disciplines, generate and return the medal table.



-- The results of the disciplines are given as a String[] results, where each element is in the format "GGG SSS BBB". GGG, SSS and BBB are the 3-letter country codes (three capital letters from 'A' to 'Z') of the countries winning the gold, silver and bronze medal, respectively.



-- The medal table is a String[] with an element for each country appearing in results. Each element has to be in the format "CCO G S B" (quotes for clarity), where G, S and B are the number of gold, silver and bronze medals won by country CCO, e.g. "AUT 1 4 1". The numbers should not have any extra leading zeros. 

-- Sort the elements by the number of gold medals won in decreasing order. If several countries are tied, sort the tied countries by the number of silver medals won in decreasing order. If some countries are still tied, sort the tied countries by the number of bronze medals won in decreasing order. If a tie still remains, sort the tied countries by their 3-letter code in ascending alphabetical order.
 
-- Definition
      
-- Class:   MedalTable
-- Method:  generate
-- Parameters: String[]
-- Returns: String[]
-- Method signature: String[] generate(String[] results)
-- (be sure your method is public)
    
 
-- Constraints
-- -  results contains between 1 and 50 elements, inclusive.
-- -  Each element of results is formatted as described in the problem statement.
-- -  No more than 50 different countries appear in results.
 
-- Examples
-- 0) 
      
-- {"ITA JPN AUS", "KOR TPE UKR", "KOR KOR GBR", "KOR CHN TPE"}
-- Returns: 
-- { "KOR 3 1 0",
--  "ITA 1 0 0",
--  "TPE 0 1 1",
--  "CHN 0 1 0",
--  "JPN 0 1 0",
--  "AUS 0 0 1",
--  "GBR 0 0 1",
--  "UKR 0 0 1" }
-- These are the results of the archery competitions.
-- 1) 
      
-- {"USA AUT ROM"}
-- Returns: { "USA 1 0 0",  "AUT 0 1 0",  "ROM 0 0 1" }
-- 2) 
      
-- {"GER AUT SUI", "AUT SUI GER", "SUI GER AUT"}
-- Returns: { "AUT 1 1 1",  "GER 1 1 1",  "SUI 1 1 1" }

