
blackAndRed xs = head $ filter (\y -> winninghand (cutDeck y xs)) zs
   where zs = [0 .. ((length xs)-1)]


winninghand xs = winninghandH xs 0 0
   where winninghandH [] m n = if m >= n then True else False
         winninghandH (x:xs) b r
            | r > b = False
            | x=='B' = winninghandH xs (b+1) r
            | x=='R' = winninghandH xs b (r+1)


cutDeck a as = (drop a as) ++ (take a as) 



-- TopCoder problem "BlackAndRed" used in SRM 223 (Division II Level Two)

-- Problem Statement
    	
-- You are playing a game with a deck of cards, half red and half black. After your friend thoroughly shuffles the cards, you cut the deck. Your friend then begins to turn the cards over one at a time, starting from the top of the deck. If, at any time, there are more red cards showing than black cards, you lose. If this never occurs by they time all the cards have been turned up, you win.
-- After losing several times in a row, you get frustrated and decide to cheat. After your friend shuffles the deck, you distract him and then secretly look at the cards, to figure out where you should cut the deck.
-- "Cutting the deck" means to take a stack of cards off the top of the deck, and place this stack on the bottom.
-- Given a String deck, representing the cards, return the number of cards you should remove from the top of the deck when you cut. A 'R' indicates a red card, and a 'B' indicates a black card. The first character of deck represents the top card, and the last character represents the bottom card. If there are multiple possibilities, return the smallest number.
 
-- Definition
    	
-- Class:	BlackAndRed
-- Method:	cut
-- Parameters:	String
-- Returns:	int
-- Method signature:	int cut(String deck)
-- (be sure your method is public)
    
 
-- Constraints
-- -	cards will contain between 2 and 50 characters, inclusive.
-- -	cards will contain only the characters 'R' and 'B'.
-- -	cards will contain the same number of 'R' and 'B' characters.
 
-- Examples
-- 0)	
    	
-- "BRBRBR"
-- Returns: 0
-- You could cut the deck at positions 0, 2, or 4 to win.
-- 1)	
    	
-- "RBRBRB"
-- Returns: 1
-- If you cut the deck at position 0, the first card dealt will be red and you will lose immediately. However, if you cut the deck at position 1, you will win.
-- 2)	
    	
-- "BBBRRRRB"
-- Returns: 7
-- The only way to win is to cut the deck at position 7. After this cut, the deck becomes: "BBBBRRRR".
-- 3)	
    	
-- "BR"
-- Returns: 0
-- 4)	
    	
-- "RBRBBRRRRBBBRBBRRBRBBRRRBRBBBRBRBRBRBRRB"
-- Returns: 9