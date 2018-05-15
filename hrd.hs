import qualified Data.Array as A
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

-- The contents of a square on the game board.
data Square = Empty 
            | Single
            | TwoTop | TwoBottom
            | TwoLeft | TwoRight
            | FourTL | FourTR | FourBL | FourBR
              deriving (Eq, Ord, Show)

-- Spaces occupied by a game piece, given its upper-left corner relative to its upper-left corner.
whole Single = [(0, 0)]
whole TwoTop = [(0, 0), (0, 1)]
whole TwoLeft = [(0, 0), (1, 0)]
whole FourTL = [(0, 0), (1, 0), (0, 1), (1, 1)]
whole _ = []

-- Vector addition.
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Initial game board postions.
mkStartPos :: [Square] -> A.Array (Int, Int) Square
mkStartPos state = A.array ((0, 0), (3, 4)) $ zip (foldr (++) [] rows) state
  where rows = [[(x, y) | x <- [0 .. 3]] | y <- [0 .. 4]]
startPos = mkStartPos [TwoTop, FourTL, FourTR, TwoTop,
                       TwoBottom, FourBL, FourBR, TwoBottom,
                       TwoTop, TwoLeft, TwoRight, TwoTop,
                       TwoBottom, Single, Single, TwoBottom,
                       Single, Empty, Empty, Single]



