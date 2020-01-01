module AdjacencySolver where
    
import System.Random
import Control.Monad.State
import Data.Set as Set
import Data.Map.Strict as Map
import RandState
import Control.Applicative
import Data.List as List

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
instance Show Tile where
    show t = "Tile: " ++ show (allowedNeighbors t)

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

newSolution :: Board -> Palette -> Either String Solution
newSolution brd plt = 
    let ts = tiles plt
        mpoptions = listSatisfies (not . Set.null) [
            Set.fromList [ti | ti <- [0..(length ts - 1)], fitsInto (ts !! ti) $ shape slot]
            | slot <- slots brd]
    in case mpoptions of
        Left s -> Left s
        Right poptions -> 
            let unsolved = Set.fromList [pi | pi <- [0..(length poptions - 1)], Set.size (poptions !! pi) > 1]
            in Right $ Solution {
                palette  = plt,
                board    = brd,
                poptions = poptions,
                unsolved = unsolved
            }

-- given a list of values, return Nothing if any do not satisfy the function
listSatisfies :: (a -> Bool) -> [a] -> Either String [a]
listSatisfies f [] = Right []
listSatisfies f (a:as) = case (f a) of
    False -> Left "failed to satisfy condition"
    True -> let mas = listSatisfies f as in
        case mas of
            Left s -> Left s
            _ -> Right (a:as)

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

pickRand :: [a] -> RandState a
pickRand ls = (ls!!) <$> (RandState $ \gen -> randomR (0, (length ls)-1) gen)
-- pickRand ls = pure $ ls !! 0

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

subtractOptions :: SlotIndex -> Set TileIndex -> Solution -> Either String Solution
subtractOptions psi remts sol = 
    let psops    = (poptions sol) !! psi
        diff     = Set.difference psops remts
        optCount = Set.size diff
    in  if (optCount == 0)
        then Left $ "failed; no options left at " ++ show psi     -- no possible tiles
        else if (optCount == Set.size psops)
            then Right sol   -- no change
            else 
                let neighbs  = neighbors ((slots . board $ sol) !! psi)
                in  subOptDirHelper (keys neighbs) (neighbs, diff, remts) (setpops sol psi diff)

subOptDirHelper :: [Direction] -> (Map Direction SlotIndex, Set TileIndex, Set TileIndex) -> Solution -> Either String Solution
subOptDirHelper [] _ sol = Right sol
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
        else subOptDirHelper dirs args =<< (subtractOptions (ninds ! dir) remdNOpts sol)

intersectOptions :: SlotIndex -> Set TileIndex -> Solution -> Either String Solution
intersectOptions index ntis sol = subtractOptions index (Set.difference ((poptions sol) !! index) ntis) sol

a # (i, t) = a >>= intersectOptions i (Set.singleton t)
a % (i, ts) = a >>= subtractOptions i (Set.fromList ts)

isSolved :: Solution -> Bool
isSolved sol = Set.null . unsolved $ sol

solveR :: Solution -> RandState (Either String Solution)
solveR sol = if (isSolved sol)
    then pure $ Right sol
    else do
        psi <- pickRand $ Set.toList (unsolved sol)      -- random unsolved pslot's index
        let pops = Set.toList ((poptions sol) !! psi)  -- available tiles for selected pslot
            weights = (\ti -> weight ((tiles . palette $ sol) !! ti)) <$> pops -- weights of tiles
        solveTileHelper sol psi =<< ((pops!!)<$>) <$> weightedShuffle weights

solveTileHelper :: Solution -> SlotIndex -> [TileIndex] -> RandState (Either String Solution)
solveTileHelper sol psi [] = pure (Left $ "ran out of tiles to test at " ++ show psi ++ "\n" ++ (show sol))
solveTileHelper sol psi (t:ts) = 
    let msol' = intersectOptions psi (Set.fromList [t]) sol
    in case msol' of
        Left s     -> solveTileHelper sol psi ts
        Right sol' -> solveR sol'

run :: Board -> Palette -> IO (String)
run brd pal = do
    gen <- newStdGen
    let res = do
        sol <- newSolution brd pal
        evalRand (solveR sol) gen
    case res of
        Right sol -> pure $ "[" ++ (intercalate ", " [show $ (Set.toList s) !! 0 |  s<-poptions sol]) ++ "]"
        Left l -> pure l
