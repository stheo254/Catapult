# Catapult
## Rules

The game is played on a 10 x 10 chessboard with 17 pieces in two colors for each player (15 knights, 1 flag, 1 general per side). At the beginning of the game, only the knights and the general are placed on the board.
Starting Position

    White begins by placing their flag on the edge of the board behind their knights, but not on the corner squares.
    Black follows and places their flag on a square on the opposite edge of the board, also not on the corner squares.
    Flags remain on their chosen squares and cannot be moved during the game.

### Movement of Knights

    Regular knights can move one step either straight forward or sideways, as long as the destination square is unoccupied.
    Knights can capture opposing pieces that are directly in front of them.
    Additionally, knights can capture diagonally to the front-left or front-right.

### Knight Retreat

If one of your knights (s1) is threatened by an opposing knight (s2), it may be able to retreat.

    Threatened means that s2 is on an adjacent square and could capture s1 on the next turn.
    During a retreat, s1 can move exactly two squares backward (either straight or diagonally), opposite to its usual movement direction.
    This is only possible if:
        The destination square is empty.
        The movement is not blocked by another piece on the square it passes through.

### Movement of the General

The general can move one step in any direction: vertically, horizontally, or diagonally, as long as the destination square is unoccupied.

    The general cannot capture any pieces.

### Catapults

When three knights are aligned in a straight or diagonal row, they form a catapult.

    A catapult can fire a shot at an opposing piece if the target is two or three squares away along the catapult's axis.
    Each catapult has two ends and can fire in both directions.
    Knights can be part of multiple catapults simultaneously.

### Rules for firing a catapult:

    A catapult shot counts as a turn, even though no pieces are moved.
    The catapult can only target a square occupied by an opposing piece.
    The catapult can only fire if the general is within range to give the firing command.

### General's range:

    The general is in range if at least one knight in the catapult is on a square the general could move to (if it were free).
    In other words, the general must be on a square adjacent to the catapult to authorize firing.

### Moving a Catapult

Instead of firing, a catapult can shift by one square along its axis.

    The destination square must be empty.
    The catapult can move in either direction, allowing a retreat from a threatening position.
    This movement does not require the general’s approval.

### Gameplay Rules

    The player whose turn it is must make a move. Passing is not allowed.
    After a move (knight movement, general movement, or catapult shot), the opponent takes their turn.

### Winning Conditions

### A player wins the game if any of the following occurs:

    Their knight captures the opponent's flag.
    Their catapult destroys the opponent's flag.
    The opponent has no valid moves left.
    Their knight captures the opponent's general.
    Their catapult hits and destroys the opponent's general.

## Test Commands:
- Run all tests: stack test
- Run validation tests: stack test catapult:validate
- Run unit tests: stack test catapult:units
- Run grading tests: stack test catapult:grading
- Run tests with coverage: stack test --coverage ...

## Building Commands
- stack build
- stack clean
