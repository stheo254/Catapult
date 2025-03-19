module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char
import Data.List.Split

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = White | Black deriving Show
data Cell = Empty | General Player | Soldier Player | Flag Player deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) White White = True
  (==) Black Black = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Soldier p1) (Soldier p2) = p1 == p2
  (==) (General p1) (General p2) = p1 == p2
  (==) (Flag p1) (Flag p2) = p1 == p2
  (==) _ _ = False


-- ##############################################################################
-- ################## IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- #############################################################################
-- chat gpt page 1 for splitOn function
validateFEN :: String -> Bool
validateFEN line = let parts = splitOn "/" line
                    in length parts == 10 && validateRow parts

validateRow :: [String]-> Bool
validateRow line = let x = map countNonNumbers line
                       y = map countTotalInt line
                       in checkSums x y
-- anjing ini gw lupa msh list of strings ganti dl counttotalint ama count non numbers b4 count g tw bsa jd buat both apa g
-- chatgpt page 5 and further on i forgot in order to fix for multiple number
convertInt :: String -> [Int]
convertInt [] = []
convertInt test = map (\x -> read [x] :: Int) (filter isDigit test)
-- chatgpt page 40 for elem function
countNonNumbers :: String -> Int
countNonNumbers test = let chars = filter (\x->not(isDigit x) && elem x "bwgBWG") test 
                      in length chars


countTotalInt :: String -> Int
countTotalInt xd = let converted = convertInt xd
                    in sum converted

checkSums :: [Int] -> [Int] -> Bool
checkSums [] [] = True
checkSums (x:xs) (y:ys)
          | x + y == 0 = checkSums xs ys
          | x + y /= 10 = False
          | otherwise = checkSums xs ys


-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 2 Functional Points                   ###################
-- ################## - 1 Coverage Point                      ###################
-- ##############################################################################
-- msh salah
buildBoard :: String -> Board
buildBoard xd = let test = splitOn "/" xd in parseRows test

parseRows :: [String] -> Board
parseRows = map parseRowH

parseRowH :: String -> [Cell]
parseRowH x = if null x then replicate 10 Empty else parseRow x

parseRow :: String -> [Cell]
parseRow [] = []
parseRow (x:xs)
  | isDigit x = replicate (chartoInt x) Empty ++ parseRow xs
  | x == 'g' = General White : parseRow xs
  | x == 'G' = General Black : parseRow xs
  | x == 'W' = Flag White : parseRow xs
  | x == 'B' = Flag Black : parseRow xs
  | x == 'w' = Soldier White : parseRow xs
  | x == 'b' = Soldier Black : parseRow xs
  | otherwise = parseRow xs
--chatgpt page 42-44
chartoInt :: Char -> Int
chartoInt x = ord x - ord '0'
  