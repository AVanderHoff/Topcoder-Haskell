
huffmanDecoding ys xs = filter (\x -> x/='8') (huffmanDecodingI qs ys 1 (length ys) )
    where qs = zip xs ['A' .. 'Z']

huffmanDecodingI xs ys b c 
     | b==c = ""
     | elem (take b ys) qs = (huffmanDecodingII xs (take b ys) ((length xs)-1)) : huffmanDecodingI xs (drop b ys) 1 c
     | otherwise = huffmanDecodingI xs ys (b+1) c
         where qs = map fst xs

huffmanDecodingII _ _ (-1) = '8'
huffmanDecodingII  xs ys a 
    | (fst(xs !! a ))== ys = snd (xs !! a)
    | otherwise = huffmanDecodingII xs ys (a-1)








-- TopCoder problem "HuffmanDecoding" used in SRM 308 (Division I Level One , Division II Level Two)

-- Problem Statement
    	
-- When text is encoded using Huffman codes, each symbol is replaced by a string of 0s and 1s called a bit string representation. The replacement is done in such a way that the bit string representation of a symbol is never the prefix of the bit string representation of any other symbol. This property allows us to unambiguously decode the encoded text.
-- You will be given a String archive and a String[] dictionary. The i-th element of dictionary will be the bit string representation of the i-th uppercase letter. Decode archive using dictionary and return the result as a single String.
 
-- Definition
    	
-- Class:	HuffmanDecoding
-- Method:	decode
-- Parameters:	String, String[]
-- Returns:	String
-- Method signature:	String decode(String archive, String[] dictionary)
-- (be sure your method is public)
    
 
-- Constraints
-- -	archive will contain between 1 and 50 characters, inclusive.
-- -	archive will contain only the characters '0' (zero) and '1' (one).
-- -	dictionary will contain between 1 and 26 elements, inclusive.
-- -	Each element of dictionary will contain between 1 and 50 characters, inclusive.
-- -	Each element of dictionary will contain only the characters '0' (zero) and '1' (one).
-- -	No element of dictionary will be a prefix of any other element of dictionary.
-- -	archive will be decodable using dictionary
 
-- Examples
-- 0)	
    	
-- "101101"
-- {"00","10","01","11"}
-- Returns: "BDC"
-- Because there are no elements in dictionary that are prefixes of other elements, only one element of dictionary will be a prefix of archive. In this case, it is the second element ("10") which represents 'B'. The rest of the text can be decoded using the same logic.
-- 1)	
    	
-- "10111010"
-- {"0","111","10"}
-- Returns: "CBAC"
-- Note that elements of dictionary can be of different lengths.
-- 2)	
    	
-- "0001001100100111001"
-- {"1","0"}
-- Returns: "BBBABBAABBABBAAABBA"
-- '1' is replaced by 'A', '0' is replaced by 'B'.
-- 3)	
    	
-- "111011011000100110"
-- {"010","00","0110","0111","11","100","101"}
-- Returns: "EGGFAC"
-- 4)	
    	
-- "001101100101100110111101011001011001010"
-- {"110","011","10","0011","00011","111","00010","0010","010","0000"}
-- Returns: "DBHABBACAIAIC"