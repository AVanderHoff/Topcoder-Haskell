findComma [] = 0
findComma (x:xs)
   |x==',' = 0
   |otherwise = 1+findComma xs
findSpace [] = 0
findSpace (x:xs)
   |x==' ' = 0
   |otherwise = 1+findSpace xs

splitName xs
   |elem ',' xs = ((drop ((findComma xs)+2) xs) , (take (findComma xs) xs))
   |otherwise =  ((take (findSpace xs) xs) , (drop ((findSpace xs)+1) xs))

clientQuickSort [] = []
clientQuickSort ((a,b):xs) = smaller ++ [(a,b)] ++ larger
   where smaller = clientQuickSort [ (c,d) | (c,d) <- xs , compareClientLess (a,b) (c,d)]
         larger = clientQuickSort [(e,f) | (e,f) <- xs , compareClientMore (a,b) (e,f)]
         compareClientLess (x,y) (w,z)
            | (y>z) = True
            | (y==z) && (x>w) = True
            | (y==z) && (w>x) = False
            | (y==z) && (w==z) = False
            | otherwise = False
         compareClientMore (m,n) (o,p)
            | (p>n) = True
            | (p==n) && (o>m) = True
            | (p==n) && (m>o) = False
            | (p==n) && (m==o) = True
            | otherwise = False

stripTupleClient :: ([Char],[Char])->[Char]
stripTupleClient (a,b) = a++ " " ++ b

clientList xs =  map stripTupleClient $ clientQuickSort $ map splitName xs




-- TopCoder problem "ClientsList" used in SRM 227 (Division I Level One , Division II Level Two)

-- Problem Statement
      
-- Your company has just undergone some software upgrades, and you will now be storing all of the names of your clients in a new database. Unfortunately, your existing data is inconsistent, and cannot be imported as it is. You have been tasked with writing a program to cleanse the data.
-- You are given a String[], names, which is a list of the names of all of your existing clients. Some of the names are in "First Last" format, while the rest are in "Last, First" format. You are to return a String[] with all of the names in "First Last" format, sorted by last name, then by first name.
 
-- Definition
      
-- Class:   ClientsList
-- Method:  dataCleanup
-- Parameters: String[]
-- Returns: String[]
-- Method signature: String[] dataCleanup(String[] names)
-- (be sure your method is public)
    
 
-- Constraints
-- -  names will contain between 1 and 50 elements, inclusive.
-- -  Each element of names will be of the form "First Last" or "Last, First" (quotes added for clarity).
-- -  Each first and last name will begin with a single capital letter 'A'-'Z', and the remaining letters will be lower case 'a'-'z'.
-- -  Each element of names will contain between 3 and 50 characters, inclusive.
 
-- Examples
-- 0) 
      
-- {"Joe Smith", "Brown, Sam", "Miller, Judi"}
-- Returns: { "Sam Brown",  "Judi Miller",  "Joe Smith" }
-- The last names, in order, are Brown, Miller, Smith. We rearrange each name to be in the proper format also.
-- 1) 
      
-- {"Campbell, Phil", "John Campbell", "Young, Warren"}
-- Returns: { "John Campbell",  "Phil Campbell",  "Warren Young" }
-- Notice here how we sort by first name when the last names are the same.
-- 2) 
      
-- {"Kelly, Anthony", "Kelly Anthony", "Thompson, Jack"}
-- Returns: { "Kelly Anthony",  "Anthony Kelly",  "Jack Thompson" }
-- Be careful to properly identify first name versus last name!
-- 3) 
      
-- {"Trevor Alvarez", "Jackson, Walter", "Mandi Stuart",
--  "Martin, Michael", "Peters, Tammy", "Richard Belmont",
--  "Carl Thomas", "Ashton, Roger", "Jamie Martin"}
-- Returns: 
-- { "Trevor Alvarez",
--  "Roger Ashton",
--  "Richard Belmont",
--  "Walter Jackson",
--  "Jamie Martin",
--  "Michael Martin",
--  "Tammy Peters",
--  "Mandi Stuart",
--  "Carl Thomas" }
-- 4) 
      
-- {"Banks, Cody", "Cody Banks", "Tod Wilson"}
-- Returns: { "Cody Banks",  "Cody Banks",  "Tod Wilson" }
-- Notice that two identical names can appear in the list.
-- 5) 
      
-- {"Mill, Steve", "Miller, Jane"}
-- Returns: { "Steve Mill",  "Jane Miller" }