
type Location = (Char, Int)

data Player = White | Black deriving (Show, Eq)

data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)

type Board = (Player, [Piece], [Piece])


setBoard :: Board

setBoard = (White,
  [R ('h', 1), N ('g', 1), B ('f', 1), K ('e', 1), Q ('d', 1), B ('c', 1), N ('b', 1), R ('a', 1)]
  ++ [P (col, 2) | col <- ['a'..'h']],
  [R ('h', 8), N ('g', 8), B ('f', 8), K ('e', 8), Q ('d', 8), B ('c', 8), N ('b', 8), R ('a', 8)]
  ++ [P (col, 7) | col <- ['a'..'h']])

  

findCell :: Board -> Int -> Char -> String

findCell (b,[],[]) row col = "    |" 

findCell (b,(P (c,r):t),black) prow pcol = if c == pcol && r == prow then " PW |" else findCell (b,t,black) prow pcol
findCell (b,(N (c,r):t),black) prow pcol = if c == pcol && r == prow then " NW |" else findCell (b,t,black) prow pcol
findCell (b,(Q (c,r):t),black) prow pcol = if c == pcol && r == prow then " QW |" else findCell (b,t,black) prow pcol
findCell (b,(K (c,r):t),black) prow pcol = if c == pcol && r == prow then " KW |" else findCell (b,t,black) prow pcol
findCell (b,(B (c,r):t),black) prow pcol = if c == pcol && r == prow then " BW |" else findCell (b,t,black) prow pcol
findCell (b,(R (c,r):t),black) prow pcol = if c == pcol && r == prow then " RW |" else findCell (b,t,black) prow pcol

findCell (b,[],(P (c,r):t)) prow pcol = if c == pcol && r == prow then " PB |" else findCell (b,[],t) prow pcol
findCell (b,[],(N (c,r):t)) prow pcol = if c == pcol && r == prow then " NB |" else findCell (b,[],t) prow pcol
findCell (b,[],(Q (c,r):t)) prow pcol = if c == pcol && r == prow then " QB |" else findCell (b,[],t) prow pcol
findCell (b,[],(K (c,r):t)) prow pcol = if c == pcol && r == prow then " KB |" else findCell (b,[],t) prow pcol
findCell (b,[],(B (c,r):t)) prow pcol = if c == pcol && r == prow then " BB |" else findCell (b,[],t) prow pcol
findCell (b,[],(R (c,r):t)) prow pcol = if c == pcol && r == prow then " RB |" else findCell (b,[],t) prow pcol





visualizeBoard:: Board->String

visualizeBoard (p,a1,a2) = "  a    b    c    d     e    f    g    h  "++ "\n" ++ visRowHelper (p,a1,a2) 8 ++ "\n" ++ "Turn: " ++(show p) 

visRowHelper :: Board -> Int -> String

visRowHelper b 0=""
visRowHelper b row  = (show row) ++ "|" ++ visColHelper b row 'a' ++ "\n" ++ visRowHelper b (row-1)

visColHelper b row 'h' = findCell b row 'h' 
visColHelper b row c = findCell b row c ++ visColHelper b row (succ c)


isValidLoc row col= row<=8&&row>=1&& elem col ['a'..'h']



isLegalPawn (P (col,row)) board (newCol,newRow) =
    if isValidLoc newRow newCol then
        if findCell board row col == " PB |" then
            if newCol == col then
                if row == 7 && newRow == 5 then -- first move
                    if findCell board newRow newCol == "    |" && findCell board (newRow+1) newCol == "    |" then -- cell fadya
                        True
                    else -- msh fadya
                        False
                else -- msh first aw 1 move first
                    if row == newRow+1 then
                        if findCell board newRow newCol == "    |" then -- fadya
                            True
                        else -- too far
                            False
                    else
                        False
            else -- hayakol
                if (col == (succ newCol) || col == (pred newCol)) && row == newRow+1 then -- ely gambaha
                    if elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |"] then -- taking enemy
                        True
                    else
                        False
                else -- msh gambaha
                    False
        else -- white
		if findCell board row col == " PW |" then
            if newCol == col then
                if row == 2 && newRow == 4 then -- first move
                    if findCell board newRow newCol == "    |" && findCell board (newRow-1) newCol == "    |" then -- cell fadya
                        True
                    else -- msh fadya
                        False
                else -- msh first aw 1 move first
                    if row == newRow-1 then
                        if findCell board newRow newCol == "    |" then -- fadya
                            True
                        else -- too far
                            False
                    else
                        False
            else -- hayakol
                if (col == (succ newCol) || col == (pred newCol)) && row == newRow-1 then -- ely gambaha
                    if elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |"] then -- taking enemy
                        True
                    else
                        False
                else -- msh gambaha
                    False
		else -- not in the specified initial Location
			False
    else -- location bara el board
        False
		

isLegalKing (K (col,row)) board (newCol,newRow) = 
    if isValidLoc newRow newCol then -- valid new location
        if findCell board row col == " KB |" then -- Black king
            if ((col==newCol && row==newRow+1) || (col==newCol && row==newRow-1) || (row==newRow && col==succ newCol) || (row==newRow && col==pred newCol) || (row==newRow-1 && col==succ newCol) || (row==newRow-1 && col==pred newCol) || (row==newRow+1 && col==succ newCol) || (row==newRow+1 && col==pred newCol)) then -- adjacent
                if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then
                    True
                else -- occupied by an ally
                    False
            else -- not adjacent
                False
        else -- white
		if findCell board row col == " KW |" then
            if ((col==newCol && row==newRow+1) || (col==newCol && row==newRow-1) || (row==newRow && col==succ newCol) || (row==newRow && col==pred newCol) || (row==newRow-1 && col==succ newCol) || (row==newRow-1 && col==pred newCol) || (row==newRow+1 && col==succ newCol) || (row==newRow+1 && col==pred newCol)) then -- adjacent
                if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then
                    True
                else -- occupied by an ally
                    False
            else -- not adjacent
                False
		else -- not in the Specified initial Location
			False
    else
        False
		
		
		
		
emptyRows board col row finalRow = 
	if (row == finalRow) then -- Base Case
		True
	else -- Recursive Case
		if (row > finalRow) then -- hatnzl
			if ((findCell board row col) == "    |") then
				emptyRows board col (row-1) finalRow
			else -- msh fadya
				False
		else -- hattetl3
			if (findCell board row col) == "    |" then
				emptyRows board col (row+1) finalRow
			else
				False
				
				
emptyCols board row col finalCol = 
	if (col == finalCol) then --Base Case
		True
	else -- Recursive Case
		if (col > finalCol) then -- raii7 shemal
			if (findCell board row col)== "    |" then 
				emptyCols board row (pred col) finalCol
			else -- msh fadya
				False
		else -- shemal
			if (findCell board row col) == "    |" then
					emptyCols board row (succ col) finalCol
			else
				False
		
isLegalRook (R (col,row)) board (newCol,newRow) = 
	if isValidLoc newRow newCol then -- valid new location
		if findCell board row col == " RB |" then -- Black Rook
			if col == newCol then --moved Vertically 
				if (row > newRow) then -- vertically Down
					if (emptyRows board col (row-1) newRow) then -- taree2 salek
						if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then -- last cell either empty or enemy
							True
						else -- last cell friendly fire
							False
					else -- msh salek
						False
				else -- Vertically Up
					if (emptyRows board col (row+1) newRow) then -- taree2 salek
						if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then -- last cell either empty or enemy
							True
						else -- last cell friendly fire
							False
					else -- msh salek
						False
			else -- not Vertically
				if (row == newRow) then -- moved Horizontally
					if (col > newCol) then -- left
						if (emptyCols board row (pred col) newCol) then -- taree2 salek
							if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then -- last cell either empty or enemy
								True
							else -- last cell friendly fire
								False
						else -- msh salek
							False
					else -- yemeen
						if (emptyCols board row (succ col) newCol) then -- taree2 salek
							if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then -- last cell either empty or enemy
								True
							else -- last cell friendly fire
								False
						else -- msh salek
							False
				else -- not Vertically nor Horizontally
					False
		else -- White Rook
			if (findCell board row col == " RW |") then
						if col == newCol then --moved Vertically 
					if (row > newRow) then -- vertically Down
					if (emptyRows board col (row-1) newRow) then -- taree2 salek
						if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then -- last cell either empty or enemy
							True
						else -- last cell friendly fire
							False
					else -- msh salek
						False
				else -- Vertically Up
					if (emptyRows board col (row+1) newRow) then -- taree2 salek
						if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then -- last cell either empty or enemy
							True
						else -- last cell friendly fire
							False
					else -- msh salek
						False
			else -- not Vertically
				if (row == newRow) then -- moved Horizontally
					if (col > newCol) then -- left
						if (emptyCols board row (pred col) newCol) then -- taree2 salek
							if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then -- last cell either empty or enemy
								True
							else -- last cell friendly fire
								False
						else -- msh salek
							False
					else -- yemeen
						if (emptyCols board row (succ col) newCol) then -- taree2 salek
							if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then -- last cell either empty or enemy
								True
							else -- last cell friendly fire
								False
						else -- msh salek
							False
				else -- not Vertically nor Horizontally
					False
			else -- not in the Specified initial Location
				False
	else -- outside board
		False


isLegalBishop (B (col,row)) board (newCol,newRow) = 
    if isValidLoc newRow newCol then -- valid new location
        if abs (newRow-row) == abs ((charToInt (newCol))-(charToInt (col)))then--cell is diagonal
            if clearBishPath board row col newRow newCol then--path fadya
                if findCell board row col == " BB |" then--  black bish
                    if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then
                        True
                    else--ally
                        False
                else--White
					if findCell board row col == " BW |" then
						if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then
							True
						else -- occupied by an ally
							False
					else -- Neither black nor white
						False
            else-- unclear path
                False

        else--not daiagonal
            False
    else--not a valid location
        False



clearBishPath board row1 col1 row2 col2= 
    if abs (row1-row2) ==1 && abs ((charToInt (col1))-(charToInt (col2)))==1 then
        True
    else
        if (row1>row2) && (col1>col2) then --t7t shmal
            ((findCell board (row1-1) (pred col1))=="    |" ) && clearBishPath board (row1-1) (pred col1) row2 col2
        else
            if (row1>row2) && (col1<col2) then --ta7t ymeen
                ((findCell board (row1-1) (succ col1))=="    |" ) && clearBishPath board (row1-1) (succ col1) row2 col2
            else 
                if (row1<row2) && (col1>col2) then --fo2 shemal
                    ((findCell board (row1+1) (pred col1))=="    |" ) && clearBishPath board (row1+1) (pred col1) row2 col2
                else 
                    ((findCell board (row1+1) (succ col1))=="    |" ) && clearBishPath board (row1+1) (succ col1) row2 col2



charToInt 'a'=1
charToInt 'b'=2
charToInt 'c'=3
charToInt 'd'=4
charToInt 'e'=5
charToInt 'f'=6
charToInt 'g'=7
charToInt 'h'=8




isLegalKnight (N (col,row)) board (newCol,newRow) = 
	if isValidLoc newRow newCol then -- valid new location
		if (abs (row-newRow) == 2 && abs ((charToInt col) - (charToInt newCol)) == 1) || (abs (row-newRow) == 1 && abs ((charToInt col) - (charToInt newCol)) == 2) then
			 if findCell board row col == " NB |" then--  black knight
                    if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then
                        True
                    else--ally
                        False
                else--White
					if findCell board row col == " NW |" then
						if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then
							True
						else -- occupied by an ally
							False
					else -- Neither black nor white
						False
		else
			False
	else
		False

isLegalQueen (Q (col,row)) board (newCol,newRow) =
	if isValidLoc newRow newCol then
		if abs (newRow-row) == abs ((charToInt (newCol))-(charToInt (col)))then--moves like a bishop
			 if clearBishPath board row col newRow newCol then--path fadya
				if findCell board row col == " QB |" then--  black queen
                    if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then
                        True
                    else--ally
                        False
                else--White
					if findCell board row col == " QW |" then
						if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then
							True
						else -- occupied by an ally
							False
					else -- Neither black nor white
						False
			 else
				False
		else--not like a bishop 
			if (row==newRow)||( col==newCol) then --like a rook
				if findCell board row col == " QB |" then -- Black queen
			if col == newCol then --moved Vertically 
				if (row > newRow) then -- vertically Down
					if (emptyRows board col (row-1) newRow) then -- taree2 salek
						if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then -- last cell either empty or enemy
							True
						else -- last cell friendly fire
							False
					else -- msh salek
						False
				else -- Vertically Up
					if (emptyRows board col (row+1) newRow) then -- taree2 salek
						if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then -- last cell either empty or enemy
							True
						else -- last cell friendly fire
							False
					else -- msh salek
						False
			else -- not Vertically
				if (row == newRow) then -- moved Horizontally
					if (col > newCol) then -- left
						if (emptyCols board row (pred col) newCol) then -- taree2 salek
							if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then -- last cell either empty or enemy
								True
							else -- last cell friendly fire
								False
						else -- msh salek
							False
					else -- yemeen
						if (emptyCols board row (succ col) newCol) then -- taree2 salek
							if (elem (findCell board newRow newCol) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |","    |"]) then -- last cell either empty or enemy
								True
							else -- last cell friendly fire
								False
						else -- msh salek
							False
				else -- not Vertically nor Horizontally
					False
		else -- White queen
			if (findCell board row col == " QW |") then-- white queen
						if col == newCol then --moved Vertically 
					if (row > newRow) then -- vertically Down
					if (emptyRows board col (row-1) newRow) then -- taree2 salek
						if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then -- last cell either empty or enemy
							True
						else -- last cell friendly fire
							False
					else -- msh salek
						False
				else -- Vertically Up
					if (emptyRows board col (row+1) newRow) then -- taree2 salek
						if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then -- last cell either empty or enemy
							True
						else -- last cell friendly fire
							False
					else -- msh salek
						False
			else -- not Vertically
				if (row == newRow) then -- moved Horizontally
					if (col > newCol) then -- left
						if (emptyCols board row (pred col) newCol) then -- taree2 salek
							if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then -- last cell either empty or enemy
								True
							else -- last cell friendly fire
								False
						else -- msh salek
							False
					else -- yemeen
						if (emptyCols board row (succ col) newCol) then -- taree2 salek
							if (elem (findCell board newRow newCol) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |","    |"]) then -- last cell either empty or enemy
								True
							else -- last cell friendly fire
								False
						else -- msh salek
							False
				else -- not Vertically nor Horizontally
					False
			else -- not in the Specified initial Location
				False
			else-- wala rook wala bish
				False
			
			
		
	else
		False




isLegal:: Piece -> Board -> Location -> Bool

isLegal (P loc) board newLoc = isLegalPawn (P loc) board newLoc
isLegal (K loc) board newLoc = isLegalKing (K loc) board newLoc
isLegal (R loc) board newLoc = isLegalRook (R loc) board newLoc
isLegal (B loc) board newLoc = isLegalBishop (B loc) board newLoc
isLegal (N loc) board newLoc = isLegalKnight (N loc) board newLoc
isLegal (Q loc) board newLoc = isLegalQueen (Q loc) board newLoc



suggestMove:: Piece -> Board -> [Location]
suggestMove piece board = helperSuuggest piece board ([(col,row) | col <- ['a'..'h'] , row <- [1..8]])


helperSuuggest piece board []=[]
helperSuuggest piece board (h:t)=
	if isLegal piece board h then
		(h:helperSuuggest piece board t)
	else	
		helperSuuggest piece board t


move:: Piece -> Location -> Board -> Board


move piece (newCol, newRow) (White, w, b) =
	if (elem (findCell (White, w, b) (getRow (getLocation piece)) (getCol (getLocation piece))) [" PW |"," RW |"," BW |"," KW |"," QW |"," NW |"])then--bam el bam
		if (elem (newCol, newRow) (suggestMove piece (White, w, b))) then -- legal move  
			if elem (findCell (White, w, b) newRow newCol) [" PB |", " RB |", " BB |", " KB |", " QB |", " NB |"] then -- hay take 
				(Black, updateMap piece (newCol, newRow) w, taken (newCol, newRow) b)
			else 
				(Black, updateMap piece (newCol, newRow) w, b) -- mafeesh 7aga to take
      
		else error ("Illegal move for piece "++(show piece))
	else
		error " This is White player’s turn, Black can’t move."
		
move piece (newCol, newRow) (Black, w, b) =
	if (elem (findCell (Black, w, b) (getRow (getLocation piece)) (getCol (getLocation piece))) [" PB |"," RB |"," BB |"," KB |"," QB |"," NB |"])then--bam el bam
		if (elem (newCol, newRow) (suggestMove piece (Black, w, b))) then -- legal move  
			if elem (findCell (Black, w, b) newRow newCol) [" PW |", " RW |", " BW |", " KW |", " QW |", " NW |"] then -- hay take 
				(White, taken (newCol, newRow) w,  updateMap piece (newCol, newRow) b)
			else 
				(White, w,  updateMap piece (newCol, newRow) b) -- mafeesh 7aga to take
      
		else error ("Illegal move for piece "++(show piece))
	else
		error " This is White player’s turn, Black can’t move."
  

		
		
updateMap piece newLoc [] = []
updateMap piece newLoc (h:t) = 
	if (piece == h) then 
		[setLocation piece newLoc] ++ t
	else 
		[h] ++ updateMap piece newLoc t  
		
taken (col,row) [] = []
taken (col,row) (h:t) = 
	if ((getLocation h) == (col,row)) then --sheelo
		t
	else
		[h] ++ taken (col,row) t
		
		
	

getCol (col,row)=col
getRow (col,row)=row
-- Getter function for location
getLocation :: Piece -> Location
getLocation (P loc) = loc
getLocation (N loc) = loc
getLocation (K loc) = loc
getLocation (Q loc) = loc
getLocation (R loc) = loc
getLocation (B loc) = loc

-- Setter function for location
setLocation :: Piece -> Location -> Piece
setLocation (P _) loc = P loc
setLocation (N _) loc = N loc
setLocation (K _) loc = K loc
setLocation (Q _) loc = Q loc
setLocation (R _) loc = R loc
setLocation (B _) loc = B loc


