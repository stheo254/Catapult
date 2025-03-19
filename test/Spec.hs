-- #############################################################################
-- ###### GRADING TESTS                                               ##########
-- #############################################################################

import Test.Hspec

import Board
     (validateFEN,
      buildBoard,
      Player(White, Black),
      Cell(Empty, Flag, Soldier, General),
      Pos(..))

import Catapult (Move(Move), playerWon, flagMoves, generalMoves, soldierMoves, catapultMoves, listMoves)

main :: IO ()
main = hspec $ do
    testValidateFEN
    testValidateBuildBoard
    testFlagMoves
    testGeneralMoves
    testSoldierMoves
    testCatapultMoves
    testPlayerWon
    testListMoves

testValidateFEN :: Spec
testValidateFEN = describe "IF Validate-Module-Board: validateFEN ..." $ do
    it "IF empty string is not valid" $ do
            validateFEN "" `shouldBe` (False :: Bool)
    it "only slashes = True" $ do
        validateFEN "/////////" `shouldBe` True
    it "only numbers valid= Treu" $ do
        validateFEN "/91/235/82/424/////" `shouldBe` True
    it "only numbers invalid = False" $ do
        validateFEN "///44//232////" `shouldBe` False
    it "only no flags valid = True" $ do
        validateFEN "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/" `shouldBe` True
    it "full string valid = True" $ do
        validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/5B4" `shouldBe` True
    it "full string not valid too many in row 6 = False" $ do
        validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/3w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/5B4" `shouldBe` False
    it "full string not valid too few in row 6 = False" $ do
        validateFEN "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/5B4" `shouldBe` False
    it "full string not valid wrong char = False" $ do
        validateFEN "4W5/1x1w1w1w1w/1w1w1w1w1w/3w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/5B4" `shouldBe` False
        
testValidateBuildBoard :: Spec
testValidateBuildBoard = describe "IF Validate-Module-Board: buildBoard ..." $ do
        it "test example board" $ do
            buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2" `shouldBe` sampleBoard
        it "test white soldier" $ do
            buildBoard "wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww/wwwwwwwwww" `shouldBe` allWhiteSoldier
        it "test black soldier" $ do
            buildBoard "bbbbbbbbbb/bbbbbbbbbb/bbbbbbbbbb/bbbbbbbbbb/bbbbbbbbbb/bbbbbbbbbb/bbbbbbbbbb/bbbbbbbbbb/bbbbbbbbbb/bbbbbbbbbb" `shouldBe` allBlackSoldier
        it "test WHITE General" $ do
            buildBoard "gggggggggg/gggggggggg/gggggggggg/gggggggggg/gggggggggg/gggggggggg/gggggggggg/gggggggggg/gggggggggg/gggggggggg" `shouldBe` allWhiteGeneral
        it "test black general" $ do
            buildBoard "GGGGGGGGGG/GGGGGGGGGG/GGGGGGGGGG/GGGGGGGGGG/GGGGGGGGGG/GGGGGGGGGG/GGGGGGGGGG/GGGGGGGGGG/GGGGGGGGGG/GGGGGGGGGG" `shouldBe` allBlackGeneral
        it "test white and black flags" $ do
            buildBoard "WWWWWWWWWW/55/55/55/55/55/55/55/55/BBBBBBBBBB" `shouldBe` boardFlagOnly

testFlagMoves:: Spec
testFlagMoves = describe "IF Validate-Module-Catapult: flagMoves ..." $ do
        it "town already placed" $ do
            flagMoves sampleBoard White `shouldBe` []
        it "white flag is already placed" $ do
            flagMoves (buildBoard "3W7/////////") White `shouldBe` []
        it "black flag is already placed" $ do
            flagMoves (buildBoard "3W6/////////2B8") Black `shouldBe` []
        it "black flag when white is not placed" $ do
            flagMoves (buildBoard "/////////") Black `shouldBe` []
        it "white flag has not been placed and valid" $ do
            flagMoves (buildBoard "/////////") White `shouldBe` possibleWhiteMoves
        it "black flag has not been placed and valid" $ do
            flagMoves (buildBoard "1W8/////////") Black `shouldBe` possibleBlackMoves
        it "white turn and invalid config" $ do
            flagMoves (buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/2B7") White `shouldBe` []

testGeneralMoves:: Spec
testGeneralMoves = describe "IF Validate-Module-Catapult: generalMoves ..." $ do
        it "wrong position" $ do
            generalMoves sampleBoard White (Pos 'a' 9) `shouldBe` []
        it "empty space for white" $ do
            generalMoves (buildBoard "19/19/19/19/19/19/19/19/19/19") White (Pos 'f' 5) `shouldMatchList` []
        it "no white general" $ do
            generalMoves (buildBoard "1W8/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5G4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b2b/") White (Pos 'f' 5) `shouldMatchList` []
        it "no black general" $ do
            generalMoves (buildBoard "1W8/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4g5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b2b/") Black (Pos 'e' 4) `shouldMatchList` []
        it "glack general surrounded with empty" $ do
            generalMoves (buildBoard "19/19/19/19/4G5/46/19/19/19/19") Black (Pos 'e' 5) `shouldMatchList` generalBlackMove
        it "white general surrounded with empty" $ do
            generalMoves (buildBoard "19/19/19/19/5g4/19/19/19/19/19") White (Pos 'f' 5) `shouldMatchList` generalWhiteMove
        it "white gneeral blocked left right white soldier" $ do
            generalMoves (buildBoard "19/19/19/19/4wgw3/19/19/19/19/19") White (Pos 'f' 5) `shouldMatchList` blockedLeftRight
        it "black gneeral blocked left right black soldier" $ do
            generalMoves (buildBoard "19/19/19/19/4bGb3/19/19/19/19/19") Black (Pos 'f' 5) `shouldMatchList` blockedLeftRight
        it "black gneeral blocked left white soldier right black soldier" $ do
            generalMoves (buildBoard "19/19/19/19/4wGb3/19/19/19/19/19") Black (Pos 'f' 5) `shouldMatchList` blockedLeftRight
        it "corner" $ do
            generalMoves (buildBoard "19/19/19/19/19/19/19/19/19/G9") Black (Pos 'a' 0) `shouldMatchList` corner

generalWhiteMove = [Move (Pos 'f' 5) (Pos 'g' 5), Move (Pos 'f' 5) (Pos 'e' 5), Move (Pos 'f' 5) (Pos 'e' 6),
    Move (Pos 'f' 5) (Pos 'e' 4), Move (Pos 'f' 5) (Pos 'f' 4), Move (Pos 'f' 5) (Pos 'g' 4),
    Move (Pos 'f' 5) (Pos 'f' 6), Move (Pos 'f' 5) (Pos 'g' 6)]  
generalBlackMove = [Move (Pos 'e' 5) (Pos 'e' 6), Move (Pos 'e' 5) (Pos 'f' 5), Move (Pos 'e' 5) (Pos 'd' 5),
                     Move (Pos 'e' 5) (Pos 'd' 4), Move (Pos 'e' 5) (Pos 'f' 4), Move (Pos 'e' 5) (Pos 'd' 6)
                  , Move (Pos 'e' 5) (Pos 'f' 6), Move (Pos 'e' 5) (Pos 'e' 4)]  
blockedLeftRight = [Move (Pos 'f' 5) (Pos 'e' 4), Move (Pos 'f' 5) (Pos 'g' 6), Move (Pos 'f' 5) (Pos 'e' 6),
                    Move (Pos 'f' 5) (Pos 'f' 6),Move (Pos 'f' 5) (Pos 'f' 4),Move (Pos 'f' 5) (Pos 'g' 4)]  
corner = [Move (Pos 'a' 0) (Pos 'b' 1),
   Move (Pos 'a' 0) (Pos 'a' 1),
   Move (Pos 'a' 0) (Pos 'b' 0)]

testSoldierMoves:: Spec
testSoldierMoves = describe "IF Validate-Module-Catapult: soldierMoves ..." $ do
        it "wrong position" $ do
            soldierMoves sampleBoard White (Pos 'a' 9) `shouldBe` []
        it "normal White movement" $ do
            soldierMoves (buildBoard "19/19/19/3w6/19/19/19/19/19/19") White (Pos 'd' 6) `shouldMatchList` normalWhiteSoldier
        it "normal Black movement" $ do
            soldierMoves (buildBoard "19/19/19/3b6/19/19/19/19/19/19") Black (Pos 'd' 6) `shouldMatchList` normalBlackSoldier
        it "attack White and retreat movement" $ do
            soldierMoves (buildBoard "19/19/19/3w6/2bbb5/19/19/19/19/19") White (Pos 'd' 6) `shouldMatchList` attackWhiteSoldier
        it "attack Black and reteeat movement" $ do
            soldierMoves (buildBoard "19/19/2www5/3b6/19/19/19/19/19/19") Black (Pos 'd' 6) `shouldMatchList` attackBlackSoldier
        it "blocked White movement" $ do
            soldierMoves (buildBoard "19/19/19/2www5/3w6/19/19/19/19/19") White (Pos 'd' 6) `shouldMatchList` []
        it "blocked Black movement" $ do
            soldierMoves (buildBoard "19/19/3b6/2bbb5/19/19/19/19/19/19") Black (Pos 'd' 6) `shouldMatchList` []
        it "partial black movement" $ do
            soldierMoves (buildBoard "19/19/316/21bb5/19/19/19/19/19/19") Black (Pos 'd' 6) `shouldMatchList`  partialBlackSoldier
        it "partail White movement" $ do
            soldierMoves (buildBoard "19/19/19/2www5/316/19/19/19/19/19") White (Pos 'd' 6) `shouldMatchList` [Move (Pos 'd' 6) (Pos 'd' 5)]
        it "white retreat movement" $ do
            soldierMoves (buildBoard "19/19/19/2www5/2wbw5/19/19/19/19/19") White (Pos 'd' 6) `shouldMatchList` retreatWhiteSoldier
        it "Black reteeat movement" $ do
            soldierMoves (buildBoard "19/19/2bwb5/2bbb5/19/19/19/19/19/19") Black (Pos 'd' 6) `shouldMatchList` retreatBlackSoldier
        it "white retreat semi blocked movement" $ do
            soldierMoves (buildBoard "19/19/31w5/2www5/2wbw5/19/19/19/19/19") White (Pos 'd' 6) `shouldMatchList` [Move (Pos 'd' 6) (Pos 'd' 5),
                                                                                                                  Move (Pos 'd' 6) (Pos 'b' 8),
                                                                                                                  Move (Pos 'd' 6) (Pos 'd' 8)]
        it "Black reteeat semi blocked movement" $ do
            soldierMoves (buildBoard "19/19/2bwb5/2bbb5/31b5/19/19/19/19/19") Black (Pos 'd' 6) `shouldMatchList` [Move (Pos 'd' 6) (Pos 'd' 7),
                                                                                                                  Move (Pos 'd' 6) (Pos 'b' 4),
                                                                                                                  Move (Pos 'd' 6) (Pos 'd' 4)]
normalWhiteSoldier = [
   Move (Pos 'd' 6) (Pos 'c' 6),
   Move (Pos 'd' 6) (Pos 'e' 6),
   Move (Pos 'd' 6) (Pos 'd' 5)]
normalBlackSoldier = [
   Move (Pos 'd' 6) (Pos 'c' 6),
   Move (Pos 'd' 6) (Pos 'e' 6),
   Move (Pos 'd' 6) (Pos 'd' 7)]
partialBlackSoldier= [
   Move (Pos 'd' 6) (Pos 'c' 6),
   Move (Pos 'd' 6) (Pos 'd' 7)]
attackWhiteSoldier = [
   Move (Pos 'd' 6) (Pos 'c' 6),
   Move (Pos 'd' 6) (Pos 'e' 6),
   Move (Pos 'd' 6) (Pos 'c' 5),
   Move (Pos 'd' 6) (Pos 'd' 5),
   Move (Pos 'd' 6) (Pos 'e' 5),
   Move (Pos 'd' 6) (Pos 'b' 8),
   Move (Pos 'd' 6) (Pos 'd' 8),
   Move (Pos 'd' 6) (Pos 'f' 8)]
attackBlackSoldier = [
   Move (Pos 'd' 6) (Pos 'c' 6),
   Move (Pos 'd' 6) (Pos 'e' 6),
   Move (Pos 'd' 6) (Pos 'c' 7),
   Move (Pos 'd' 6) (Pos 'd' 7),
   Move (Pos 'd' 6) (Pos 'e' 7),
   Move (Pos 'd' 6) (Pos 'b' 4),
   Move (Pos 'd' 6) (Pos 'd' 4),
   Move (Pos 'd' 6) (Pos 'f' 4)]
retreatWhiteSoldier = [
   Move (Pos 'd' 6) (Pos 'd' 5),
   Move (Pos 'd' 6) (Pos 'b' 8),
   Move (Pos 'd' 6) (Pos 'd' 8),
   Move (Pos 'd' 6) (Pos 'f' 8)]
retreatBlackSoldier = [
   Move (Pos 'd' 6) (Pos 'd' 7),
   Move (Pos 'd' 6) (Pos 'b' 4),
   Move (Pos 'd' 6) (Pos 'd' 4),
   Move (Pos 'd' 6) (Pos 'f' 4)]
testCatapultMoves:: Spec
testCatapultMoves = describe "IF Validate-Module-Catapult: catapultMoves ..." $ do
        it "wrong position" $ do
            catapultMoves sampleBoard White (Pos 'a' 9) `shouldBe` []
        it " shift behind for white" $ do
            catapultMoves (buildBoard "19/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/19/19/19/19/19/19") White (Pos 'd' 6) `shouldMatchList` [Move (Pos 'd' 6) (Pos 'd' 9)]
        it "shift front for white" $ do
            catapultMoves (buildBoard "19/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/19/19/19/19/19/19") White (Pos 'd' 8) `shouldMatchList` [Move (Pos 'd' 8) (Pos 'd' 5)]
        it " shift front for black" $ do
            catapultMoves (buildBoard "19/1b1b1b1b1b/1b1b1b1b1b/1b1b1b1b1b/19/19/19/19/19/19") Black (Pos 'd' 6) `shouldMatchList` [Move (Pos 'd' 6) (Pos 'd' 9)]
        it "shift behind for black" $ do
            catapultMoves (buildBoard "19/1b1b1b1b1b/1b1b1b1b1b/1b1b1b1b1b/19/19/19/19/19/19") Black (Pos 'd' 8) `shouldMatchList` [Move (Pos 'd' 8) (Pos 'd' 5)]
        it "shoot and shift for black" $ do
            catapultMoves (buildBoard "19/2w7/2w7//2b7/2b7/2bG6/19/19/19") Black (Pos 'c' 3) `shouldMatchList` 
                [Move (Pos 'c' 3) (Pos 'c' 7),Move (Pos 'c' 3) (Pos 'c' 8),Move (Pos 'c' 3) (Pos 'c' 6)]
        it "shoot and shift general adjacent of other for black" $ do
            catapultMoves (buildBoard "19/2w7/2w7//2bG6/2b7/2b7/19/19/19") Black (Pos 'c' 3) `shouldMatchList` 
                [Move (Pos 'c' 3) (Pos 'c' 7),Move (Pos 'c' 3) (Pos 'c' 8),Move (Pos 'c' 3) (Pos 'c' 6)]
        it "shoot general and shift general adjacent of other for black" $ do
            catapultMoves (buildBoard "19/2w7/2g7//2bG6/2b7/2b7/19/19/19") Black (Pos 'c' 3) `shouldMatchList` 
                [Move (Pos 'c' 3) (Pos 'c' 7),Move (Pos 'c' 3) (Pos 'c' 8),Move (Pos 'c' 3) (Pos 'c' 6)]
        it "shoot flag and shift general adjacent of other for black" $ do
            catapultMoves (buildBoard "19/2w7/2W7//2bG6/2b7/2b7/19/19/19") Black (Pos 'c' 3) `shouldMatchList` 
                [Move (Pos 'c' 3) (Pos 'c' 7),Move (Pos 'c' 3) (Pos 'c' 8),Move (Pos 'c' 3) (Pos 'c' 6)]
        it "no general for black" $ do
            catapultMoves (buildBoard "19/2w7/2W7//2b7/2b7/2b7/19/19/19") Black (Pos 'c' 3) `shouldMatchList` 
                [Move (Pos 'c' 3) (Pos 'c' 6)]
        it "cant shift and shoot for black" $ do
            catapultMoves (buildBoard "19/2b7/2b7/2b7/2b7/2b7/2b7/19/19/19") Black (Pos 'c' 3) `shouldMatchList` 
                []


testPlayerWon:: Spec
testPlayerWon = describe "IF Validate-Module-Catapult: playerWon ..." $ do
        it "nobody has won yet" $ do
            playerWon sampleBoard White `shouldBe` False
        it "start conf white" $ do
            playerWon (buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/") White `shouldBe` False
        it "start conf black" $ do
            playerWon (buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/") Black `shouldBe` False
        it "start conf with flag white" $ do
            playerWon (buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/5B4") White `shouldBe` False
        it "start conf with flag black" $ do
            playerWon (buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/5B4") Black `shouldBe` False
        it "noMoves white" $ do
            playerWon noMoveBoard White `shouldBe` False
        it "noMoves black" $ do
            playerWon noMoveBoard Black `shouldBe` True
        it "no flag black white wins" $ do
            playerWon (buildBoard "4W5/g9////4G5/1b1b1b1b1b/1b1b1b1b1b/1b1b1b1b1b/") White `shouldBe` True
        it "no flag black black doesnt wins" $ do
            playerWon (buildBoard "4W5/g9////4G5/1b1b1b1b1b/1b1b1b1b1b/1b1b1b1b1b/") Black `shouldBe` False
        it "no general white black wins" $ do
            playerWon (buildBoard "4W5/19////4G5/1b1b1b1b1b/1b1b1b1b1b/1b1b1b1b1b/1B8") White `shouldBe` False
        it "no generall white white doesnt wins" $ do
            playerWon (buildBoard "4W5/19////4G5/1b1b1b1b1b/1b1b1b1b1b/1b1b1b1b1b/") Black `shouldBe` True
        

testListMoves:: Spec
testListMoves = describe "IF Validate-Module-Catapult: listMoves ..." $ do
        it "no movesi white" $ do
            listMoves noMoveBoard White `shouldBe` []
        it "no moves black" $ do
            listMoves (buildBoard "1W8/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/////1B8") Black `shouldMatchList` []
        it "no moves black" $ do
            listMoves (buildBoard "1W8/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/////1B8") Black `shouldMatchList` []
        it "no moves black" $ do
            listMoves (buildBoard "1W8/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/////1B8") Black `shouldMatchList` []
        it "empty board" $ do
            listMoves emptyBoard White `shouldBe` []
        it "empty board" $ do
            listMoves emptyBoard Black `shouldBe` []
        it "singular white soldier" $ do
            listMoves (buildBoard "1W8/19/19/19/19/4w5/19/19/19/1B8") White `shouldMatchList` 
                [Move (Pos 'e' 4) (Pos 'e' 3),Move (Pos 'e' 4) (Pos 'd' 4),Move (Pos 'e' 4) (Pos 'f' 4)]
        it "singular black soldier" $ do
            listMoves (buildBoard "1B8/19/19/19/19/4b5/19/19/19/1B8") Black `shouldMatchList` 
                [Move (Pos 'e' 4) (Pos 'e' 5),Move (Pos 'e' 4) (Pos 'd' 4),Move (Pos 'e' 4) (Pos 'f' 4)]
        it "multiple white soldier" $ do
            listMoves (buildBoard "1W8/19/19/19/19/w1w1w5/19/19/19/1B8") White `shouldMatchList` 
                [Move (Pos 'e' 4) (Pos 'e' 3),Move (Pos 'e' 4) (Pos 'd' 4),Move (Pos 'e' 4) (Pos 'f' 4),
                Move (Pos 'a' 4) (Pos 'a' 3),Move (Pos 'a' 4) (Pos 'b' 4), Move (Pos 'c' 4) (Pos 'c' 3),
                Move (Pos 'c' 4) (Pos 'b' 4),Move (Pos 'c' 4) (Pos 'd' 4)]
        it "multiple black soldier" $ do
            listMoves (buildBoard "1W8/19/19/19/19/b1b1b5/19/19/19/1B8") Black `shouldMatchList` 
                [Move (Pos 'e' 4) (Pos 'e' 5),Move (Pos 'e' 4) (Pos 'd' 4),Move (Pos 'e' 4) (Pos 'f' 4),
                Move (Pos 'a' 4) (Pos 'a' 5),Move (Pos 'a' 4) (Pos 'b' 4), Move (Pos 'c' 4) (Pos 'c' 5),
                Move (Pos 'c' 4) (Pos 'b' 4),Move (Pos 'c' 4) (Pos 'd' 4)]
        it "singular black general" $ do
            listMoves (buildBoard "1W8/19/19/19/19/4G5/19/19/19/1B8") Black `shouldMatchList` 
                [Move (Pos 'e' 4) (Pos 'e' 5),Move (Pos 'e' 4) (Pos 'd' 4),Move (Pos 'e' 4) (Pos 'f' 4),
                Move (Pos 'e' 4) (Pos 'd' 5),Move (Pos 'e' 4) (Pos 'f' 5), Move (Pos 'e' 4) (Pos 'e' 3),
                Move (Pos 'e' 4) (Pos 'd' 3),Move (Pos 'e' 4) (Pos 'f' 3)]
        it "singular white general" $ do
            listMoves (buildBoard "1W8/19/19/19/19/4g5/19/19/19/1B8") White `shouldMatchList` 
                [Move (Pos 'e' 4) (Pos 'e' 5),Move (Pos 'e' 4) (Pos 'd' 4),Move (Pos 'e' 4) (Pos 'f' 4),
                Move (Pos 'e' 4) (Pos 'd' 5),Move (Pos 'e' 4) (Pos 'f' 5), Move (Pos 'e' 4) (Pos 'e' 3),
                Move (Pos 'e' 4) (Pos 'd' 3),Move (Pos 'e' 4) (Pos 'f' 3)]
        it "start conf black" $ do
            listMoves (buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1b1/b1b1b1b1b1/b1b1b1b1b1/7B2") Black `shouldMatchList` allPossibleBlackMovesStart
        it "start conf white" $ do
            listMoves (buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1bo1/b1b1b1b1b1/b1b1b1b1b1/7B2") White `shouldMatchList` allPossibleWhiteMovesStart
        it "white place flag" $ do
            listMoves (buildBoard "/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1bo1/b1b1b1b1b1/b1b1b1b1b1/") White `shouldMatchList` possibleWhiteMoves
        it "black place flag" $ do
            listMoves (buildBoard "4W5/1w1w1w1w1w/1w1w1w1w1w/1w1w1w1w1w/5g4/4G5/b1b1b1b1bo1/b1b1b1b1b1/b1b1b1b1b1/") Black `shouldMatchList` possibleBlackMoves
        it "shoot and shift for black" $ do
            listMoves (buildBoard "1W8/2w7/2w7//2b7/2b7/2bG6/19/19/1B8") Black `shouldMatchList` 
                [Move (Pos 'c' 3) (Pos 'c' 7),Move (Pos 'c' 3) (Pos 'c' 8),Move (Pos 'c' 3) (Pos 'c' 6), Move (Pos 'c' 5) (Pos 'd' 5)
                ,Move (Pos 'c' 5) (Pos 'b' 5),Move (Pos 'c' 5) (Pos 'c' 6),Move (Pos 'c' 5) (Pos 'c' 2),Move (Pos 'c' 4) (Pos 'b' 4),Move (Pos 'c' 4) (Pos 'd' 4)
                ,Move (Pos 'c' 3) (Pos 'b' 3),Move (Pos 'd' 3) (Pos 'e' 3),Move (Pos 'd' 3) (Pos 'e' 2),Move (Pos 'd' 3) (Pos 'c' 2),Move (Pos 'd' 3) (Pos 'e' 4)
                ,Move (Pos 'd' 3) (Pos 'd' 2),Move (Pos 'd' 3) (Pos 'd' 4)]
        it "no movesas flag is not here" $ do
            listMoves (buildBoard "19/2w7/2w7//2b7/2b7/19/19/19/1B8") White `shouldMatchList` []

--"/////////"
allPossibleWhiteMovesStart = [Move (Pos 'f' 5) (Pos 'f' 4), Move (Pos 'f' 5) (Pos 'g' 4)
                              , Move (Pos 'f' 5) (Pos 'e' 5), Move (Pos 'f' 5) (Pos 'g' 5)
                              , Move (Pos 'f' 5) (Pos 'e' 6), Move (Pos 'f' 5) (Pos 'g' 6) 
                              , Move (Pos 'b' 6) (Pos 'a' 6), Move (Pos 'b' 6) (Pos 'c' 6), Move (Pos 'b' 6) (Pos 'b' 5)
                              , Move (Pos 'd' 6) (Pos 'c' 6), Move (Pos 'd' 6) (Pos 'e' 6), Move (Pos 'd' 6) (Pos 'd' 5)
                              , Move (Pos 'f' 6) (Pos 'e' 6), Move (Pos 'f' 6) (Pos 'g' 6)
                              , Move (Pos 'h' 6) (Pos 'g' 6), Move (Pos 'h' 6) (Pos 'i' 6), Move (Pos 'h' 6) (Pos 'h' 5)
                              , Move (Pos 'j' 6) (Pos 'i' 6), Move (Pos 'j' 6) (Pos 'j' 5), Move (Pos 'b' 7) (Pos 'a' 7), Move (Pos 'b' 7) (Pos 'c' 7)
                              , Move (Pos 'd' 7) (Pos 'c' 7), Move (Pos 'd' 7) (Pos 'e' 7), Move (Pos 'f' 7) (Pos 'e' 7), Move (Pos 'f' 7) (Pos 'g' 7)
                              , Move (Pos 'h' 7) (Pos 'g' 7), Move (Pos 'h' 7) (Pos 'i' 7), Move (Pos 'j' 7) (Pos 'i' 7)                             
                              , Move (Pos 'b' 8) (Pos 'a' 8), Move (Pos 'b' 8) (Pos 'c' 8)
                              , Move (Pos 'd' 8) (Pos 'c' 8), Move (Pos 'd' 8) (Pos 'e' 8)
                              , Move (Pos 'f' 8) (Pos 'e' 8), Move (Pos 'f' 8) (Pos 'g' 8)
                              , Move (Pos 'h' 8) (Pos 'g' 8), Move (Pos 'h' 8) (Pos 'i' 8), Move (Pos 'j' 8) (Pos 'i' 8)
                              , Move (Pos 'b' 6) (Pos 'b' 9), Move (Pos 'd' 6) (Pos 'd' 9)
                              , Move (Pos 'f' 6) (Pos 'f' 9), Move (Pos 'h' 6) (Pos 'h' 9), Move (Pos 'j' 6) (Pos 'j' 9)
                              , Move (Pos 'b' 8) (Pos 'b' 5), Move (Pos 'd' 8) (Pos 'd' 5)
                              , Move (Pos 'h' 8) (Pos 'h' 5), Move (Pos 'j' 8) (Pos 'j' 5)]
allPossibleBlackMovesStart = [Move (Pos 'e' 4) (Pos 'd' 3), Move (Pos 'e' 4) (Pos 'f' 3), Move (Pos 'e' 4) (Pos 'd' 4), Move (Pos 'e' 4) (Pos 'f' 4), Move (Pos 'e' 4) (Pos 'd' 5), Move (Pos 'e' 4) (Pos 'e' 5)
                              , Move (Pos 'e' 3) (Pos 'f' 3), Move (Pos 'g' 3) (Pos 'f' 3), Move (Pos 'g' 3) (Pos 'h' 3), Move (Pos 'g' 3) (Pos 'g' 4), Move (Pos 'i' 3) (Pos 'h' 3), Move (Pos 'i' 3) (Pos 'j' 3), Move (Pos 'i' 3) (Pos 'i' 4)
                              , Move (Pos 'a' 3) (Pos 'b' 3), Move (Pos 'a' 3) (Pos 'a' 4), Move (Pos 'c' 3) (Pos 'b' 3), Move (Pos 'c' 3) (Pos 'd' 3), Move (Pos 'c' 3) (Pos 'c' 4), Move (Pos 'e' 3) (Pos 'd' 3)
                              , Move (Pos 'a' 1) (Pos 'b' 1), Move (Pos 'c' 1) (Pos 'b' 1), Move (Pos 'c' 1) (Pos 'd' 1), Move (Pos 'e' 1) (Pos 'd' 1), Move (Pos 'e' 1) (Pos 'f' 1), Move (Pos 'g' 1) (Pos 'f' 1)
                              , Move (Pos 'e' 2) (Pos 'd' 2), Move (Pos 'e' 2) (Pos 'f' 2), Move (Pos 'g' 2) (Pos 'f' 2), Move (Pos 'g' 2) (Pos 'h' 2), Move (Pos 'i' 2) (Pos 'h' 2), Move (Pos 'i' 2) (Pos 'j' 2)
                              , Move (Pos 'g' 1) (Pos 'h' 1), Move (Pos 'i' 1) (Pos 'h' 1), Move (Pos 'i' 1) (Pos 'j' 1)
                              , Move (Pos 'a' 1) (Pos 'a' 4), Move (Pos 'c' 1) (Pos 'c' 4), Move (Pos 'g' 1) (Pos 'g' 4), Move (Pos 'i' 1) (Pos 'i' 4), Move (Pos 'a' 3) (Pos 'a' 0)
                              , Move (Pos 'c' 3) (Pos 'c' 0), Move (Pos 'e' 3) (Pos 'e' 0), Move (Pos 'g' 3) (Pos 'g' 0), Move (Pos 'i' 3) (Pos 'i' 0)
                              , Move (Pos 'a' 2) (Pos 'b' 2), Move (Pos 'c' 2) (Pos 'b' 2), Move (Pos 'c' 2) (Pos 'd' 2)
                             ]

possibleWhiteMoves = [Move (Pos 'b' 9) (Pos 'b' 9), Move (Pos 'c' 9) (Pos 'c' 9),Move (Pos 'd' 9) (Pos 'd' 9),
                  Move (Pos 'e' 9) (Pos 'e' 9), Move (Pos 'f' 9) (Pos 'f' 9),
                  Move (Pos 'g' 9) (Pos 'g' 9), Move (Pos 'h' 9) (Pos 'h' 9),
                  Move (Pos 'i' 9) (Pos 'i' 9)]

possibleBlackMoves = [Move (Pos 'b' 0) (Pos 'b' 0), Move (Pos 'c' 0) (Pos 'c' 0),Move (Pos 'd' 0) (Pos 'd' 0),
                  Move (Pos 'e' 0) (Pos 'e' 0), Move (Pos 'f' 0) (Pos 'f' 0),
                  Move (Pos 'g' 0) (Pos 'g' 0), Move (Pos 'h' 0) (Pos 'h' 0),
                  Move (Pos 'i' 0) (Pos 'i' 0)]


sampleBoard = [
                [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
                [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
                [Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White), Empty          , (Soldier White)],
                [Empty          , Empty          , Empty          , Empty          , Empty          , (General White), Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , (General Black), Empty          , Empty          , Empty          , Empty          , Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]]

noMoveBoard = [
                [Empty          , Empty          , Empty          , Empty          , (Flag White)   , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [Empty          , Empty          , Empty          , Empty          , (General Black)          , Empty          , Empty          , Empty          , Empty          , Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [(Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          , (Soldier Black), Empty          ],
                [Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , Empty          , (Flag Black)   , Empty          , Empty          ]]
emptyBoard = [
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]


allWhiteSoldier =  
    [ [Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White],
      [Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White],
      [Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White],
      [Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White],
      [Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White],
      [Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White],
      [Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White],
      [Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White],
      [Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White],
      [Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White, Soldier White]
    ]
allBlackSoldier =  
    [ [Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black],
      [Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black],
      [Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black],
      [Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black],
      [Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black],
      [Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black],
      [Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black],
      [Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black],
      [Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black],
      [Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black, Soldier Black]
    ]
allWhiteGeneral =  
    [ [General White, General White, General White, General White, General White, General White, General White, General White, General White, General White],
      [General White, General White, General White, General White, General White, General White, General White, General White, General White, General White],
      [General White, General White, General White, General White, General White, General White, General White, General White, General White, General White],
      [General White, General White, General White, General White, General White, General White, General White, General White, General White, General White],
      [General White, General White, General White, General White, General White, General White, General White, General White, General White, General White],
      [General White, General White, General White, General White, General White, General White, General White, General White, General White, General White],
      [General White, General White, General White, General White, General White, General White, General White, General White, General White, General White],
      [General White, General White, General White, General White, General White, General White, General White, General White, General White, General White],
      [General White, General White, General White, General White, General White, General White, General White, General White, General White, General White],
      [General White, General White, General White, General White, General White, General White, General White, General White, General White, General White]
    ]
allBlackGeneral =  
    [ [General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black],
      [General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black],
      [General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black],
      [General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black],
      [General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black],
      [General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black],
      [General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black],
      [General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black],
      [General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black],
      [General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black, General Black]
    ]
boardFlagOnly = 
    [ [Flag White, Flag White, Flag White, Flag White, Flag White, Flag White, Flag White, Flag White, Flag White, Flag White],
      [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
      [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
      [Flag Black, Flag Black, Flag Black, Flag Black, Flag Black, Flag Black, Flag Black, Flag Black, Flag Black, Flag Black]
    ]