import System.Random
import Control.Monad.State
import Data.Set as Set
import Data.Map.Strict as Map
-- import RandState

type Direction = Int
type Shape = Int

data Tile = Tile {
    tileIndex :: Int,
    fitsInto :: Shape -> Bool,
    allowedNeighbors :: Map Direction (Set Int)
}

data Palette = Palette {
    tiles :: [Tile]
}

data Slot = Slot {
    slotIndex :: Int,
    shape :: Shape,
    neighbors :: Map Direction Int
} deriving Show

data Board = Board {
    slots :: [Slot]
} deriving Show

data Solution = Solution {
    palette :: Palette,
    board :: Board,
    poptions :: [Set Int],
    unsolved :: Set Int
}

newSolution :: Board -> Palette -> Maybe Solution
newSolution brd plt = 
    let ts          = tiles plt
    -- poptions = list of viable tilesets per pslot
    -- note: must be 1:1 correspondance between (board's slots) and (solution's poptions)
        mpoptions    = listSatisfies (not . Set.null) [
            Set.fromList [ti | ti <- [0..(length ts - 1)], fitsInto (ts !! ti) $ shape slot]
            | slot <- slots brd]
    in case mpoptions of
        Nothing -> Nothing
        Just poptions -> 
            let unsolved = Set.fromList [pi | pi <- [0..(length poptions - 1)], Set.size (poptions !! pi) > 1]
            in Just $ Solution {
                palette  = plt,
                board    = brd,
                poptions = poptions,
                unsolved = unsolved
            }

gridDirsN :: Int -> [(Direction, [Int])]
gridDirsN axes = [
    (sign * axis, (replicate (axis-1) 0) ++ [sign] ++ (replicate (axes-axis) 0))
    | axis  <- [1..axes]
    , sign <- [-1, 1]]

generateGrid2 :: Bool -> Int -> Int -> Board
generateGrid2 loop w h = 
    let sif = \x y -> x + y * w
        dirs2 = gridDirsN 2
    in Board $ [Slot {
        slotIndex = sif x y,
        shape = 1,
        neighbors = Map.fromList $ [
            (dir, sif nxm nym)
            | (dir, [ox, oy]) <- dirs2
            , let nx = x+ox
            , let ny = y+oy
            , let valid = 0 <= nx && nx < w && 0 <= ny && ny < h
            , loop || valid
            , let nxm = if valid then nx else mod nx w
            , let nym = if valid then ny else mod ny h]
    }
    | x <- [0..(w-1)]
    , y <- [0..(h-1)]]

-- [set(ti for ti in range(len(palette.tiles)) if palette.tiles[ti].fitsInto(slot.shape)) for slot in board.slots]

-- given a list of values, return Nothing if any do not satisfy the function
listSatisfies :: (a -> Bool) -> [a] -> Maybe [a]
listSatisfies f [] = Just []
listSatisfies f (a:as) = case (f a) of
    False -> Nothing
    True -> let mas = listSatisfies f as in
        case mas of
            Nothing -> Nothing
            _ -> Just (a:as)
            
-- randIndex :: Foldable t=> t a -> RandState Int
-- randIndex ls = RandState $ \gen -> randomR (0, (length ls)-1) gen

-- randItem :: [a] -> RandState a
-- randItem ls = (ls !!) <$> randIndex ls

-- solve :: Board -> Maybe Board
-- solve brd = 
--     let undecs = undecideds brd
--     in case (Data.Set.null undecs) of
--         True -> Just brd
--         False -> do
--             gen <- newStdGen
--             Just $ evalRand (decideRand brd) gen

-- decideRand :: Board -> RandState (Maybe Board)
-- decideRand brd = RandState $ \gen -> do
--     (slotIndex, gen') <- (randIndex $ undecideds brd)
--     undefined
    