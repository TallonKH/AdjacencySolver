import System.Random
import Control.Monad.State
import Data.Set as Set
import Data.Map.Strict as Map
import RandState
import Control.Applicative

type Direction = Int
type Shape = Int
type TileIndex = Int
type SlotIndex = Int

data Tile = Tile {
    -- tileIndex :: TileIndex,
    weight :: Float,
    fitsInto :: Shape -> Bool,
    allowedNeighbors :: Map Direction (Set TileIndex)
}

data Palette = Palette {
    tiles :: [Tile]
}

data Slot = Slot {
    -- slotIndex :: Int,
    shape :: Shape,
    neighbors :: Map Direction SlotIndex
} deriving Show

data Board = Board {
    slots :: [Slot]
} deriving Show

-- poptions is a list of viable tilesets per pslot
-- note: poptions must be 1:1 correspondance between (board's slots) and (solution's poptions)
data Solution = Solution {
    palette :: Palette,
    board :: Board,
    poptions :: [Set TileIndex],
    unsolved :: Set SlotIndex
}
instance Show Solution where
    show sol = "Solution { \n\tBoard: " ++ show (board sol) ++ "\n\tpoptions: " ++ show (poptions sol) ++ ",\n\tunsolved: " ++ show (unsolved sol) ++ "\n}"

newSolution :: Board -> Palette -> Maybe Solution
newSolution brd plt = 
    let ts = tiles plt
        mpoptions = listSatisfies (not . Set.null) [
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
        shape = 1,
        neighbors = Map.fromList $ [
            (dir, sif (mod nx w) (mod ny h))
            | (dir, [ox, oy]) <- dirs2
            , let nx = x+ox
            , let ny = y+oy
            , let valid = 0 <= nx && nx < w && 0 <= ny && ny < h
            , loop || valid
            , let nxm = if valid then nx else mod nx w
            , let nym = if valid then ny else mod ny h]
    }
    | y <- [0..(h-1)]
    , x <- [0..(w-1)]]

-- given a list of values, return Nothing if any do not satisfy the function
listSatisfies :: (a -> Bool) -> [a] -> Maybe [a]
listSatisfies f [] = Just []
listSatisfies f (a:as) = case (f a) of
    False -> Nothing
    True -> let mas = listSatisfies f as in
        case mas of
            Nothing -> Nothing
            _ -> Just (a:as)

removeNth :: Int -> [a] -> [a]
removeNth n ls = Prelude.take n ls ++ Prelude.drop (n+1) ls

setNth :: Int -> a -> [a] -> [a]
setNth n m ls = Prelude.take n ls ++ [m] ++ Prelude.drop (n+1) ls

weightedShuffle :: (Fractional n, Ord n, Random n) => [n] -> RandState [Int]
weightedShuffle [] = pure []
weightedShuffle ws = weightedRandom ws >>= (\i -> (i:) <$> (weightedShuffle (removeNth i ws)))

-- given list of weights, returns index of randomly chosen one
weightedRandom :: (Fractional n, Ord n, Random n) => [n] -> RandState Int
weightedRandom wts = seekDecr wts <$> (RandState $ \gen -> randomR (0, sum wts + 0.0) gen)

seekDecr :: (Fractional n, Ord n) => [n] -> n -> Int
seekDecr [w] _ = 0
seekDecr (w:ws) counter = 
    let diff = counter - w
    in  if diff < 0
        then 0
        else 1 + (seekDecr ws diff)
seekDecr _ _ = undefined -- counter is >= sum of weights? or empty list

setpops :: Solution -> SlotIndex -> Set TileIndex -> Solution
setpops sol psi new = Solution {
    palette  = palette sol,
    board    = board sol,
    poptions = setNth psi new $ poptions sol,
    unsolved =  if (Set.size new == 1)
                then Set.delete psi $ unsolved sol
                else unsolved sol
}

subtractOptions :: SlotIndex -> Set TileIndex -> Solution -> Maybe Solution
subtractOptions psi remts sol = 
    let neighbs  = neighbors ((slots . board $ sol) !! psi)
        psops    = poptions sol !! psi
        diff     = Set.difference psops remts
        optCount = Set.size diff
    in  if (optCount == 0)
        then Nothing    -- no possible tiles
        else if (optCount == Set.size psops)
            then Just sol   -- no change
            else subOptDirHelper (keys neighbs) (neighbs, diff, remts) (setpops sol psi diff)

subOptDirHelper :: [Direction] -> (Map Direction SlotIndex, Set TileIndex, Set TileIndex) -> Solution -> Maybe Solution
subOptDirHelper [] _ sol = Just sol
subOptDirHelper (dir:dirs) args@(ninds, opts, ropts) sol = 
    let palTils = tiles . palette $ sol -- list of all tiles in palette
        noptsf = \ti -> (allowedNeighbors (palTils !! ti)) ! dir -- function to get a tile's allowed neighbors in a fixed direction
        currentNOpts =              Set.unions $ (Set.map noptsf opts)
        -- remdNOpts = actually removed options (only appear in removed, and are not still in current)
        remdNOpts = Set.difference (Set.unions $ (Set.map noptsf ropts)) currentNOpts

    in  if (Set.size remdNOpts == 0)
         -- nothing changed toward this direction - continue and check the rest
        then subOptDirHelper dirs args sol
         -- neighbor changed - recurse on it, then continue and check the rest
        else subOptDirHelper dirs args =<< subtractOptions (ninds ! dir) remdNOpts sol

intersectOptions :: SlotIndex -> Set TileIndex -> Solution -> Maybe Solution
intersectOptions index ntis sol = subtractOptions index (Set.difference ((poptions sol) !! index) ntis) sol

isSolved :: Solution -> Bool
isSolved sol = Set.null . unsolved $ sol

solveR :: Solution -> RandState (Maybe Solution)
solveR sol = if (isSolved sol)
    then pure $ Just sol
    else let psi = (Set.toList (unsolved sol)) !! 0
             pops = Set.toList ((poptions sol) !! psi)
             weights = (\ti -> weight ((tiles . palette $ sol) !! ti)) <$> pops
             shuffledOpts = weightedShuffle weights
         in solveTileHelper sol psi =<< shuffledOpts 

solveTileHelper :: Solution -> SlotIndex -> [TileIndex] -> RandState (Maybe Solution)
solveTileHelper _ _ [] = pure Nothing
solveTileHelper sol psi (t:ts) = 
    let sol' = intersectOptions psi (Set.fromList [t]) sol
    in case sol' of
        Nothing   -> solveTileHelper sol psi ts
        Just sol' -> solveR sol'

run :: Board -> Palette -> IO (Maybe Solution)
run brd pal = do
    gen <- newStdGen
    let res = do
        sol <- newSolution brd pal
        evalRand (solveR sol) gen
    pure res

testTile0 = Tile {
    weight = 1,
    fitsInto = const True,
    allowedNeighbors = Map.fromList [
        (-1, Set.fromList [0]),
        (1, Set.fromList [0]),
        (-2, Set.fromList [0,1,2]),
        (2, Set.fromList [0,1,2])]
}
testTile1 = Tile {
    weight = 1,
    fitsInto = const True,
    allowedNeighbors = Map.fromList [
        (-1, Set.fromList [1,2]),
        (1, Set.fromList [1,2]),
        (-2, Set.fromList [0,1,2]),
        (2, Set.fromList [0,1,2])]
}
testTile2 = Tile {
    weight = 1,
    fitsInto = const True,
    allowedNeighbors = Map.fromList [
        (-1, Set.fromList [1]),
        (1, Set.fromList [1]),
        (-2, Set.fromList [0,1,2]),
        (2, Set.fromList [0,1,2])]
}

testTileA = Tile {
    weight = 1,
    fitsInto = const True,
    allowedNeighbors = Map.fromList [
        (-1, Set.fromList [1]),
        (1, Set.fromList [1]),
        (-2, Set.fromList [1]),
        (2, Set.fromList [1])]
}
testTileB = Tile {
    weight = 1,
    fitsInto = const True,
    allowedNeighbors = Map.fromList [
        (-1, Set.fromList [0]),
        (1, Set.fromList [0]),
        (-2, Set.fromList [0]),
        (2, Set.fromList [0])]
}

testPal = Palette [testTile0, testTile1, testTile2]
testPal2 = Palette [testTileA, testTileB]
testGrid = generateGrid2 False 3 3
testSol = newSolution testGrid testPal