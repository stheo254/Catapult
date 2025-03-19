{-# LANGUAGE ScopedTypeVariables #-}
module Catapult where  -- do NOT CHANGE export of module
-- chatgpt is normally used in order to find some examples and check if some scenarios are possible
-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board
import Data.Char
data Move = Move {start :: Pos, target :: Pos}

instance Show Move where
  show (Move (Pos startC startR) (Pos targetC targetR)) = [startC] ++ show startR ++ "-" ++ [targetC] ++ show targetR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1)) (Move (Pos sc2 sr2) (Pos tc2 tr2)) =
    sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2

-- #################################################################################################
-- ################## IMPLEMENT flagMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

data Catapult = N | NE | E | SE | S | SW | W | NW deriving Show
instance Eq Catapult where
  (==) N N = True
  (==) NE NE = True
  (==) NW NW = True
  (==) S S = True
  (==) SE SE = True
  (==) SW SW = True
  (==) W W = True
  (==) E E = True
  (==) _ _ = False
-- player pices -> check kl ada general black, general white, 15 soldier
-- should be empty if (p goes and p flag alread yexist),(w goes and black already exist ), (black goes and white doesnt exist)
flagMoves :: Board -> Player -> [Move]
flagMoves brd p
  | checkFlag brd p = []
  | p == Black && not(checkFlag brd White) = []
  | p == White && checkFlag brd Black = []
  | not (checkState brd p) = []
  | otherwise = checkEmptyFlag brd p
--flagMoves _ _ = [(Move (Pos 'a' 9) (Pos 'a' 9))]
-- chatgpt page 49-50 to check if elem could be used on Cell
checkFlag :: Board-> Player -> Bool
checkFlag [] _ = False
checkFlag (x:xs) p
  | Flag p `elem` x = True
  | otherwise = checkFlag xs p
--chatgpt page 59 to find out how to access list
-- page 51 for tail
checkFlagEdge :: Board -> Player -> Bool
checkFlagEdge (x:xs) p
  | p == White && (Flag White == head(x) || Flag White == last(x)) = True
  | p == Black && (Flag Black == last(last(xs)) || Flag Black == last(last(xs))) = True
  | otherwise = False

checkEmptyFlag :: Board -> Player -> [Move]
checkEmptyFlag brd p
  | p == White = [Move (Pos 'b' 9) (Pos 'b' 9), Move (Pos 'c' 9) (Pos 'c' 9),Move (Pos 'd' 9) (Pos 'd' 9),
                  Move (Pos 'e' 9) (Pos 'e' 9), Move (Pos 'f' 9) (Pos 'f' 9),
                  Move (Pos 'g' 9) (Pos 'g' 9), Move (Pos 'h' 9) (Pos 'h' 9),
                  Move (Pos 'i' 9) (Pos 'i' 9)]
  | p == Black = [Move (Pos 'b' 0) (Pos 'b' 0), Move (Pos 'c' 0) (Pos 'c' 0),Move (Pos 'd' 0) (Pos 'd' 0),
                  Move (Pos 'e' 0) (Pos 'e' 0), Move (Pos 'f' 0) (Pos 'f' 0),
                  Move (Pos 'g' 0) (Pos 'g' 0), Move (Pos 'h' 0) (Pos 'h' 0),
                  Move (Pos 'i' 0) (Pos 'i' 0)]

checkState :: Board -> Player -> Bool
checkState brd p
  | compareState xd (correctState p) = compareState xdd (correctState (flipPlayer p))
  | otherwise = False
  where
    xd = playerPieces brd p
    xdd = playerPieces brd (flipPlayer p)
    
compareState :: [(Pos,Cell)] -> [(Pos, Cell)] -> Bool
compareState [] _ = True
compareState (x:xs) correct 
  | elem x correct = compareState xs correct
  | otherwise =  False
correctState :: Player -> [(Pos,Cell)]
correctState p = if p == White then correctStateWhite else correctStateBlack

correctStateWhite :: [(Pos, Cell)]
correctStateWhite = [(Pos 'b' 8, Soldier White),(Pos 'd' 8, Soldier White),(Pos 'f' 8, Soldier White),
                (Pos 'b' 7, Soldier White),(Pos 'd' 7, Soldier White),(Pos 'f' 7, Soldier White),
                (Pos 'b' 6, Soldier White),(Pos 'd' 6, Soldier White),(Pos 'f' 6, Soldier White),
                (Pos 'h' 8, Soldier White),(Pos 'h' 7, Soldier White),(Pos 'h' 6, Soldier White),
                (Pos 'j' 8, Soldier White),(Pos 'j' 7, Soldier White),(Pos 'j' 6, Soldier White), (Pos 'f' 5 , General White)]
                
correctStateBlack :: [(Pos,Cell)]
correctStateBlack = [
                (Pos 'a' 1, Soldier Black),(Pos 'c' 1, Soldier Black),(Pos 'e' 1, Soldier Black),
                (Pos 'a' 2, Soldier Black),(Pos 'c' 2, Soldier Black),(Pos 'e' 2, Soldier Black),
                (Pos 'a' 3, Soldier Black),(Pos 'c' 3, Soldier Black),(Pos 'e' 3, Soldier Black),
                (Pos 'g' 1, Soldier Black),(Pos 'g' 2, Soldier Black),(Pos 'g' 3, Soldier Black),
                (Pos 'i' 1, Soldier Black),(Pos 'i' 2, Soldier Black),(Pos 'i' 3, Soldier Black),
                (Pos 'e' 4, General Black)                ]
-- #################################################################################################
-- ################## IMPLEMENT generalMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

generalMoves :: Board -> Player -> Pos -> [Move]
--generalMoves _ _ _ = [(Move (Pos 'a' 9) (Pos 'a' 9))]
generalMoves brd p pos
  | not (checkPos brd p pos 1) = []
  |otherwise = movGeneral brd pos

--rank 0=none 1=General 2=soldier
checkPos :: Board -> Player -> Pos-> Int  -> Bool
checkPos brd p pos rank= let cPG = checkPosG brd p pos
                         in cPG == rank
checkPosG :: Board -> Player -> Pos -> Int
checkPosG [[]] _ _ = 0
checkPosG brd p pos
----  | trace("\n" ++ show brd ++"\n" ++show pos) False = undefined
  | emptyBoardH brd = 0
  | brd !! yAxis pos !! xAxisH pos == General p = 1
  | brd !! yAxis pos !! xAxisH pos == Soldier p = 2
  | otherwise = 0

emptyBoardH :: Board -> Bool
emptyBoardH [] = True
emptyBoardH (x:brd)
  | all (==Empty) x = emptyBoardH brd
  | otherwise = False
-- tinggal cari around the general blm tentu beener smua ato g

movGeneral :: Board -> Pos -> [Move]
movGeneral brd pos = let gRow = xAxisH pos
                      in movGeneralH brd (aroundGeneral gRow (yAxis pos)) pos

movGeneralH :: Board -> [[Int]]-> Pos -> [Move]
movGeneralH _ [] _= []
movGeneralH brd (x:xs) pos
  | head x == 10 || head x == -1 || last x == 10 || last x == -1 = movGeneralH brd xs pos
  | brd !! (last x)!! head x == Empty = Move pos (Pos (xAxisNormal (head x)) (yAxisNormal (last x))) : movGeneralH brd xs pos
  | otherwise = movGeneralH brd xs pos

aroundGeneral :: Int -> Int -> [[Int]]
aroundGeneral x y =[[x+1,y],[x-1,y],[x,y+1],[x,y-1],[x+1,y+1],[x-1,y+1],[x+1,y-1],[x-1,y-1]]


--
-- #################################################################################################
-- ################## IMPLEMENT soldierMoves :: Board -> Player -> Pos -> [Move] ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################
-- check field -> check player -> check if under attack -> return moves
soldierMoves :: Board -> Player -> Pos -> [Move]
soldierMoves brd p pos
  | not (checkPos brd p pos 2) = []
  | otherwise = movSoldier brd (aroundSoldier (xAxisH pos) (yAxis pos) p) p pos

isUnderAttack :: Board ->Player -> Pos -> Bool
isUnderAttack brd p pos = isUnderAttackH brd (inFront (xAxisH pos) (yAxis pos) p) p

isUnderAttackH :: Board -> [[Int]] -> Player -> Bool
isUnderAttackH _ [] _ = False
isUnderAttackH brd (x:xs) p
  | head x == 10 || head x == -1 || last x == 10 || last x == -1 = isUnderAttackH brd xs p
  | (brd !! (last x))!! head x == Soldier (flipPlayer p) = True
  | otherwise = isUnderAttackH brd xs p


isFlag :: Board -> [[Int]] -> Player -> Bool
isFlag _ [] _ = False
isFlag brd (x:xs) p
  | head x == 10 || head x == -1 || last x == 10 || last x == -1 = isFlag brd xs p
  | (brd !! (last x))!! head x == Flag (flipPlayer p) = True
  | otherwise = isFlag brd xs p

movSoldier :: Board -> [[Int]]->Player -> Pos -> [Move]
movSoldier _ [] _ _= []
movSoldier brd list p pos
  | isFlag brd (inFront (xAxisH pos) (yAxis pos) p) p = attackSoldierH brd (inFront (xAxisH pos) (yAxis pos) p) p pos ++ movSoldierH brd list p pos
  | isUnderAttack brd p pos = attackSoldierH brd (inFront (xAxisH pos) (yAxis pos) p) p pos ++ movSoldierH brd list p pos ++ reverseSoldier brd (behindSoldier (xAxisH pos) (yAxis pos) p) (blockBehind (xAxisH pos) (yAxis pos) p) pos p
  | otherwise = movSoldierH brd list p pos

movSoldierH :: Board -> [[Int]] -> Player -> Pos -> [Move]
movSoldierH _ [] _ _ = []
movSoldierH brd (x:xs) p pos
  | head x == 10 || head x == -1 || last x == 10 || last x == -1 = movSoldierH brd xs p pos
  |(brd !! (last x))!! head x == Empty = Move pos (Pos (xAxisNormal (head x)) (yAxisNormal (last x))) : movSoldierH brd xs p pos
  | otherwise = movGeneralH brd xs pos

attackSoldierH :: Board -> [[Int]] -> Player -> Pos -> [Move]
attackSoldierH _ [] _ _ = []
attackSoldierH brd (x:xs) p pos
  | head x == 10 || head x <= -1 || last x == 10 || last x == -1 = attackSoldierH brd xs p pos
  | (brd !! (last x))!! head x == Flag (flipPlayer p) = Move pos (Pos (xAxisNormal (head x)) (yAxisNormal (last x))) : attackSoldierH brd xs p pos
  | (brd !! (last x))!! head x == Soldier (flipPlayer p) = Move pos (Pos (xAxisNormal (head x)) (yAxisNormal (last x))) : attackSoldierH brd xs p pos
  | otherwise = attackSoldierH brd xs p pos
 -- ini baru in perspective of black blm w´hite 
 -- mgkin msh ada salah kl blkrng di block trs samping g di block
reverseSoldier :: Board -> [[Int]]->[[Int]] -> Pos-> Player -> [Move]
reverseSoldier _ [] _ _ _ = []
reverseSoldier _ _ [] _ _ = []
reverseSoldier brd (x:xs) (y:ys) pos p
  | head x >= 10 || head x <= -1 || last x >= 10 || last x <= -1 = reverseSoldier brd xs ys pos p
  | (brd !! (last x))!! head x == Empty && (brd !! (last y))!! head y == Empty = Move pos (Pos (xAxisNormal (head x)) (yAxisNormal (last x))) : reverseSoldier brd xs ys pos p
  | otherwise = []
--                         if (brd !! row pos) !! (x) == Empty then [Move pos (Pos (xAxisNormal x) (row pos))] else []
--reverseSoldier = letif (brd !! (xAxisH pos)-2) !! (yAxis pos) == Empty then [Move pos (Pos (xAxis))]
checkBehind :: Board -> [[Int]] -> Bool
checkBehind _ [] = False
checkBehind brd (x:xs)
  | head x >= 10 || head x <= -1 || last x >= 10 || last x <= -1 = checkBehind brd xs
  | (brd !! (last x))!! head x /= Empty = True
  |otherwise = checkBehind brd xs

inFront:: Int -> Int-> Player -> [[Int]]
inFront x y p
  | p == Black = [[x-1,y-1],[x,y-1],[x+1,y-1]]
  | p == White = [[x-1,y+1],[x,y+1],[x+1,y+1]]

aroundSoldier :: Int -> Int -> Player -> [[Int]]
aroundSoldier x y p
  | p == Black = [[x+1,y],[x-1,y],[x,y-1]]
  | p == White = [[x+1,y],[x-1,y],[x,y+1]]

behindSoldier :: Int -> Int -> Player -> [[Int]]
behindSoldier x y p
  | p == White = [[x,y-2],[x-2,y-2],[x+2,y-2]]
  | p == Black = [[x,y+2],[x-2,y+2],[x+2,y+2]]
blockBehind :: Int -> Int -> Player -> [[Int]]
blockBehind x y p
  | p == White = [[x,y-1],[x-1,y-1],[x+1,y-1]]
  | p == Black = [[x,y+1],[x-1,y+1],[x+1,y+1]]
-- #################################################################################################
-- ################## IMPLEMENT catapultMoves :: Board -> Player -> Pos -> [Move]  ###################
-- ################## - 4 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################
-- check direction -> check position of soldier ->  make sure behind the catapult -> check if enemy in the direction ->
-- everything in this section is pure total garbage idk how else to clean it or rather no time
catapultMoves :: Board -> Player -> Pos -> [Move]
catapultMoves brd p pos
--  | trace ("\ntest dir catapult" ++ show (dirCatapult brd pos p 0) ) printBoard brd = undefined
  | not (checkPos brd p pos 2) = []
-- make a check whether general is in range or not (next to soldier) then if not next to general [] blm dibikin 
  | otherwise = movCatalpult brd (dirCatapult brd pos p 0) p pos

--checkGeneralAround brd pos p ctp offset
movCatalpult :: Board -> [Catapult] -> Player -> Pos -> [Move]
movCatalpult _ [] _ _ = []
movCatalpult brd (x:xs) p pos
--  | trace ("test " ++ show x ++ " " ++ show p) False = undefined
  | p == White && elem x [S,SE,SW,W,E] && checkGeneralAround brd pos p x 0 = movementCatapult brd x p pos ++ attackCatapult2 brd x p pos ++ movCatalpult brd xs p pos
  | p == White && elem x [S,SE,SW,W,E] = movementCatapult brd x p pos ++ movCatalpult brd xs p pos
  | p == White && elem x [N,NE,NW] = movementCatapult brd x p pos ++ movCatalpult brd xs p pos
  | p == Black && elem x [N,NE,NW,W,E] && checkGeneralAround brd pos p x 0 = movementCatapult brd x p pos ++ attackCatapult2 brd x p pos ++ movCatalpult brd xs p pos
  | p == Black && elem x [N,NE,NW,W,E] = movementCatapult brd x p pos ++ movCatalpult brd xs p pos
  | p == Black && elem x [S,SE,SW] = movementCatapult brd x p pos ++ movCatalpult brd xs p pos
  |otherwise = []

movementCatapult :: Board -> Catapult -> Player -> Pos -> [Move]
movementCatapult brd ctp p pos
--  | trace ("movementCatapult " ++ show (y-3) ++ show (yAxisNormal (y-3))) False = undefined
  | ctp == N && safeAccess brd (y-3) x == Just Empty = [Move pos (Pos (xAxisNormal x) (yAxisNormal (y-3)))]
  | ctp == NE && safeAccess brd (y-3) (x+3) == Just Empty = [Move pos (Pos (xAxisNormal (x+3)) (yAxisNormal (y-3)))]
  | ctp == NW && safeAccess brd (y-3) (x-3) == Just Empty = [Move pos (Pos (xAxisNormal (x-3)) (yAxisNormal (y-3)))]
  | ctp == S && safeAccess brd (y+3) x == Just Empty = [Move pos (Pos (xAxisNormal x) (yAxisNormal (y+3)))]
  | ctp == SE && safeAccess brd (y+3) (x+3) == Just Empty = [Move pos (Pos (xAxisNormal (x+3)) (yAxisNormal (y+3)))]
  | ctp == SW && safeAccess brd (y+3) (x-3) == Just Empty = [Move pos (Pos (xAxisNormal (x-3)) (yAxisNormal (y+3)))]
  | ctp == E && safeAccess brd y (x+3) == Just Empty = [Move pos (Pos (xAxisNormal (x+3)) (yAxisNormal y))]
  | ctp == W && safeAccess brd y (x-3) == Just Empty = [Move pos (Pos (xAxisNormal (x-3)) (yAxisNormal y))]
  | otherwise = []
  where
    x = xAxisH pos
    y = yAxis pos

attackCatapult2 :: Board -> Catapult -> Player -> Pos -> [Move]
attackCatapult2 brd ctp p pos
  | ctp == N && safeAccess brd (y-4) x `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = Move pos (Pos (xAxisNormal x) (yAxisNormal (y-4))): attackCatapult3 brd ctp p pos
  | ctp == NE && safeAccess brd (y-4) (x-4) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = Move pos (Pos (xAxisNormal (x-4)) (yAxisNormal (y-4))): attackCatapult3 brd ctp p pos
  | ctp == NW && safeAccess brd (y-4) (x+4) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = Move pos (Pos (xAxisNormal (x+4)) (yAxisNormal (y-4))): attackCatapult3 brd ctp p pos
  | ctp == S && safeAccess brd (y+4) x `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = Move pos (Pos (xAxisNormal x) (yAxisNormal (y+4))): attackCatapult3 brd ctp p pos
  | ctp == SE && safeAccess brd (y+4) (x-4) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = Move pos (Pos (xAxisNormal (x-4)) (yAxisNormal (y+4))): attackCatapult3 brd ctp p pos
  | ctp == SW && safeAccess brd (y+4) (x+4) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = Move pos (Pos (xAxisNormal (x+4)) (yAxisNormal (y+4))): attackCatapult3 brd ctp p pos
  | ctp == E && safeAccess brd y (x-4) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = Move pos (Pos (xAxisNormal (x-4)) (yAxisNormal y)): attackCatapult3 brd ctp p pos
  | ctp == W && safeAccess brd y (x+4) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = Move pos (Pos (xAxisNormal (x+4)) (yAxisNormal y)): attackCatapult3 brd ctp p pos
  | otherwise = []
  where
  x = xAxisH pos
  y = yAxis pos

attackCatapult3 :: Board -> Catapult -> Player -> Pos -> [Move]
attackCatapult3 brd ctp p pos
  | ctp == N && safeAccess brd (y-5) x `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = [Move pos (Pos (xAxisNormal x) (yAxisNormal (y-5)))]
  | ctp == NE && safeAccess brd (y-5) (x-5) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = [Move pos (Pos (xAxisNormal (x-5)) (yAxisNormal (y-5)))]
  | ctp == NW && safeAccess brd (y-5) (x+5) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = [Move pos (Pos (xAxisNormal (x+5)) (yAxisNormal (y-5)))]
  | ctp == S && safeAccess brd (y+5) x `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = [Move pos (Pos (xAxisNormal x) (yAxisNormal (y+5)))]
  | ctp == SE && safeAccess brd (y+5) (x-5) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = [Move pos (Pos (xAxisNormal (x-5)) (yAxisNormal (y+5)))]
  | ctp == SW && safeAccess brd (y+5) (x+5) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = [Move pos (Pos (xAxisNormal (x+5)) (yAxisNormal (y+5)))]
  | ctp == E && safeAccess brd y (x-5) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = [Move pos (Pos (xAxisNormal (x-5)) (yAxisNormal y))]
  | ctp == W && safeAccess brd y (x+5) `elem` [Just (Soldier (flipPlayer p)), Just (Flag (flipPlayer p)), Just (General (flipPlayer p))] = [Move pos (Pos (xAxisNormal (x+5)) (yAxisNormal y))]
  | otherwise = []
  where
    x = xAxisH pos
    y = yAxis pos
--N NE E SE S SW W NW
--idk how to check if  out of bounds
--ini bakal never ending no?
--THIS IS SO SHIT IDK WHAT IM DOING
--SHIT HAS SO MANY PARAMETERS THAT IM DROWNING IN IT

dirCatapult :: Board -> Pos -> Player -> Int -> [Catapult]
dirCatapult brd pos p xd
--  |trace ("dirCatapult" ++ show xd ++ show (safeAccess brd (yAxis pos - 1) (xAxisH pos) )++ show (Just(Soldier p)) ++ show (yAxis pos - 1)) False = undefined
  | xd >= 8 = []
  | safeAccess brd (yAxis pos - 1) (xAxisH pos) ==  Just (Soldier p) && xd == 0 = dirCatapultH brd [yAxis pos - 1, xAxisH pos] pos p N xd
  | safeAccess brd (yAxis pos - 1) (xAxisH pos + 1) ==  Just (Soldier p)&& xd == 1 = dirCatapultH brd [yAxis pos - 1, xAxisH pos + 1] pos p NE xd
  | safeAccess brd (yAxis pos) (xAxisH pos + 1) ==  Just (Soldier p)&& xd == 2 = dirCatapultH brd [yAxis pos , xAxisH pos + 1] pos p E xd
  | safeAccess brd (yAxis pos + 1) (xAxisH pos + 1) ==  Just (Soldier p)&& xd == 3 = dirCatapultH brd [yAxis pos + 1, xAxisH pos +1] pos p SE xd
  | safeAccess brd (yAxis pos + 1) (xAxisH pos) ==  Just (Soldier p)&& xd == 4 = dirCatapultH brd [yAxis pos + 1, xAxisH pos] pos p S xd
  | safeAccess brd (yAxis pos + 1) (xAxisH pos - 1) ==  Just (Soldier p)&& xd == 5 = dirCatapultH brd [yAxis pos + 1, xAxisH pos - 1] pos p SW xd
  | safeAccess brd (yAxis pos ) (xAxisH pos - 1) ==  Just (Soldier p) && xd == 6 = dirCatapultH brd [yAxis pos , xAxisH pos - 1] pos p W xd
  | safeAccess brd (yAxis pos - 1) (xAxisH pos - 1) ==  Just (Soldier p) && xd == 7 = dirCatapultH brd [yAxis pos - 1, xAxisH pos - 1] pos p NW xd
  |otherwise = dirCatapult brd pos p (xd + 1)

dirCatapultH :: Board -> [Int] -> Pos -> Player -> Catapult-> Int -> [Catapult]
dirCatapultH brd (x:xs) pos p dir xd
--  |trace ("dirCatapultH" ++ show xd ++ show (safeAccess brd (head xs -1) (x) )++ show (Just(Soldier p)) ++ show (x:xs)++ show (head xs - 1)) False = undefined
  | xd >= 8 = []
  | xd == 0 && safeAccess brd (x - 1) (head xs ) ==  Just (Soldier p) = N: dirCatapult brd pos p 1
  | xd == 1 && safeAccess brd (x - 1) (head xs + 1) ==  Just (Soldier p) = NE: dirCatapult brd pos p 2
  | xd == 2 && safeAccess brd x (head xs + 1) ==  Just (Soldier p) = E:dirCatapult brd pos p 3
  | xd == 3 && safeAccess brd (x + 1) (head xs + 1) ==  Just (Soldier p) = SE :dirCatapult brd pos p 4
  | xd == 4 && safeAccess brd (x + 1) (head xs) ==  Just (Soldier p) = S: dirCatapult brd pos p 5
  | xd == 5 && safeAccess brd (x + 1) (head xs - 1) ==  Just (Soldier p) = SW: dirCatapult brd pos p 6
  | xd == 6 && safeAccess brd x  (head xs - 1) ==  Just (Soldier p) = W: dirCatapult brd pos p 7
  | xd == 7 && safeAccess brd (x - 1) (head xs - 1) ==  Just (Soldier p) = NW: dirCatapult brd pos p 8
  | otherwise = dirCatapult brd pos p (xd+1)
dirCatapultH _ _ _ _ _ _ = []

safeAccess :: Board -> Int -> Int -> Maybe Cell
safeAccess brd y x
--  | trace "test2" False = undefined
  | x < 0 || y < 0 || x >9 || y > 9 = Nothing
  | otherwise = Just (brd !! y !! x)

checkGeneralAround :: Board -> Pos-> Player -> Catapult ->Int -> Bool
checkGeneralAround brd pos p ctp offset
  | offset >2 = False
  |checkGeneralAroundH brd (aroundGeneral x y) p pos = True
  | ctp == N && checkGeneralAroundH brd (aroundGeneral x (y-offset)) p pos = True
  | ctp == S && checkGeneralAroundH brd (aroundGeneral x (y+offset)) p pos = True
  | ctp == E && checkGeneralAroundH brd (aroundGeneral (x+ offset) y) p pos = True
  | ctp == W && checkGeneralAroundH brd (aroundGeneral (x-offset) (y)) p pos = True
  | ctp == NW && checkGeneralAroundH brd (aroundGeneral (x- offset) (y-offset)) p pos = True
  | ctp == NE && checkGeneralAroundH brd (aroundGeneral (x+ offset) (y-offset)) p pos = True
  | ctp == SE && checkGeneralAroundH brd (aroundGeneral (x+offset) (y+offset)) p pos = True
  | ctp == SW && checkGeneralAroundH brd (aroundGeneral (x-offset) (y+offset)) p pos = True
  |otherwise = checkGeneralAround brd pos p ctp (offset +1)
  where
    y = yAxis pos
    x = xAxisH pos

checkGeneralAroundH :: Board -> [[Int]]-> Player -> Pos -> Bool
checkGeneralAroundH _ [] _ _ = False
checkGeneralAroundH brd (x:xs) p pos
  | safeAccess brd y xY == Just (General p) = True
  |otherwise = checkGeneralAroundH brd xs p pos
  where
    xY = head x
    y = last x
-- #################################################################################################
-- ################## IMPLEMENT playerWon :: Board -> Maybe Player               ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################

playerWon :: Board -> Player -> Bool
playerWon brd p
  | buildBoard("/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1bo1/b1b1b1b1b1/b1b1b1b1b1/") == brd = False
  | not(any (==(Flag (flipPlayer p))) (concat brd)) = True
  | not(any (==(General (flipPlayer p))) (concat brd)) = True
  | null (listMoves brd (flipPlayer p)) = True
  |otherwise = False


-- #################################################::################################################
-- ################## IMPLEMENT listMoves :: Board -> Player -> [Move]           ###################
-- ################## - 2 Functional Points                                      ###################
-- ################## - 1 Coverage Point                                         ###################
-- #################################################################################################
-- help from chatgpt at page 102
listMoves :: Board -> Player -> [Move]
listMoves brd p
  | null x = []
  | checkFlag brd p == False = flagMoves brd p 
  | checkFlagEdge brd p== True = []
  | otherwise = x
  where
   x = concatMap (pieceMoves brd p) (playerPieces brd p)

--  help from chatgpt at page 107 until 113 playerPieces
playerPieces :: Board -> Player -> [(Pos,Cell)]
playerPieces brd p=
  [(Pos (xAxisNormal x) (yAxisNormal y), cell) | 
   (y, row) <- zip [0..] brd, 
   (x, cell) <- zip [0..] row, playerCheck p cell
   ]
  
playerCheck :: Player -> Cell -> Bool
playerCheck p cell = case cell of
                      (Soldier px) -> p == px
                      (General px) -> p == px
--                      (Flag px) -> p == px
                      _ -> False

pieceMoves :: Board -> Player -> (Pos, Cell) -> [Move]
pieceMoves brd p (pos, cell)  
  | cell == (Soldier p) = soldierMoves brd p pos ++ catapultMoves brd p pos
  | cell == (General p) = generalMoves brd p pos
--  | cell == (Flag p) = flagMoves brd p
  | otherwise = []


yAxis :: Pos -> Int
yAxis pos
  | row pos == 9 = 0
  | row pos == 8 = 1
  | row pos == 7 = 2
  | row pos == 6 = 3
  | row pos == 5 = 4
  | row pos == 4 = 5
  | row pos == 3 = 6
  | row pos == 2 = 7
  | row pos == 1 = 8
  | row pos == 0 = 9
  |otherwise = -1

yAxisNormal :: Int -> Int
yAxisNormal pos
  | pos == 0 = 9
  | pos == 1 = 8
  | pos == 2 = 7
  | pos == 3 = 6
  | pos == 4 = 5
  | pos == 5 = 4
  | pos == 6 = 3
  | pos == 7 = 2
  | pos == 8 = 1
  | pos == 9 = 0
  | otherwise = -1

xAxisNormal :: Int -> Char
xAxisNormal x
  | x == 0 = 'a'
  | x == 1 = 'b'
  | x == 2 = 'c'
  | x == 3 = 'd'
  | x == 4 = 'e'
  | x == 5 = 'f'
  | x == 6 = 'g'
  | x == 7 = 'h'
  | x == 8 = 'i'
  | x == 9 = 'j'
  | otherwise = '0'

xAxisH :: Pos -> Int
xAxisH x
  | col x == 'a' = 0
  | col x == 'b' = 1
  | col x == 'c' = 2
  | col x == 'd' = 3
  | col x == 'e' = 4
  | col x == 'f' = 5
  | col x == 'g' = 6
  | col x == 'h' = 7
  | col x == 'i' = 8
  | col x == 'j' = 9
  | otherwise = -1
--chatgpt page 76
flipPlayer :: Player -> Player
flipPlayer White = Black
flipPlayer Black = White

--printBoard :: Board -> Bool
--printBoard [] = False
--printBoard (x:xs) = trace (show x) printBoard xs