module GridGen3 where

import AdjacencySolver
import Data.Map.Strict as Map
import Data.Set as Set
import Data.List

gridDirsN :: Int -> [(Direction, [Int])]
gridDirsN axes = [
    (sign * axis, (replicate (axis-1) 0) ++ [sign] ++ (replicate (axes-axis) 0))
    | axis  <- [1..axes]
    , sign <- [-1, 1]]

-- generateGrid2 :: Bool -> Int -> Int -> Board
-- generateGrid2 loop w h = 
--     let sif = \x y -> x + y * w
--         dirs2 = gridDirsN 2
--     in Board $ [Slot {
--         shape = 1,
--         neighbors = Map.fromList $ [
--             (dir, sif nx ny)
--             | (dir, [ox, oy]) <- dirs2
--             , let nx = x+ox
--             , let ny = y+oy
--             , let valid = 0 <= nx && nx < w && 0 <= ny && ny < h
--             , loop || valid
--             , let nxm = if valid then nx else mod nx w
--             , let nym = if valid then ny else mod ny h]
--     }
--     | y <- [0..(h-1)]
--     , x <- [0..(w-1)]]

generateGrid3 :: Bool -> Int -> Int -> Int -> Board
generateGrid3 loop w h l = 
    let wh = w*h
        sif = \x y z -> (x) + (y * w) + (z * wh)
        dirs3 = gridDirsN 3
    in Board $ [Slot {
        shape = 1,
        neighbors = Map.fromList $ [
            (dir, sif nx ny nz)
            | (dir, [ox, oy, oz]) <- dirs3
            , let nx = x+ox
            , let ny = y+oy
            , let nz = z+oz
            , let valid = 0 <= nx && nx < w && 0 <= ny && ny < h && 0 <= nz && nz < l
            , loop || valid
            , let nxm = if valid then nx else mod nx w
            , let nym = if valid then ny else mod ny h
            , let nzm = if valid then nz else mod nz l]
    }
    | z <- [0..(l-1)]
    , y <- [0..(h-1)]
    , x <- [0..(w-1)]]

data EdgeQualifier
    = Any
    | Neighboring
    | Lonely
instance Show EdgeQualifier where
    show Any = "Any"
    show Neighboring = "Neighboring"
    show Lonely = "Lonely"

edgeQualifies :: EdgeQualifier -> (Bool -> Bool)
edgeQualifies Any = const True
edgeQualifies Neighboring = id
edgeQualifies Lonely = not

-- sidetypes are orientationless
type SideType = String

data Tile3 = Tile3 {
    weightt :: Float,
    edgeQualifiers :: [EdgeQualifier],
    sides :: [Set SideType],
    rots :: [Int],
    inverts :: [Bool] 
}
instance Show Tile3 where
    show t = "Tile {\n\t" ++ intercalate "\n\t" [
        "EdgeQs:\t" ++ (show $ edgeQualifiers t),
        "Sides:\t"  ++ (show $ sides t),
        "Rots:\t"   ++ (show $ rots t),
        "flips:\t"  ++ (show $ inverts t)] ++ "\n}"

rotateX90ls :: ([a] -> [a])
rotateX90ls = \[xn,xp,yn,yp,zn,zp] -> [xn,xp,zp,zn,yn,yp]

rotateY90ls :: ([a] -> [a])
rotateY90ls = \[xn,xp,yn,yp,zn,zp] -> [zn,zp,yn,yp,xp,xn]

rotateZ90ls :: ([a] -> [a])
rotateZ90ls = \[xn,xp,yn,yp,zn,zp] -> [yp,yn,xn,xp,zn,zp]

same :: a -> [a]
same = just id

andSame :: (a -> [a]) -> (a -> [a])
andSame f = \a -> a : (f a)

collect :: [(a -> [a])] -> (a -> [a])
collect fs = \a -> concat $ fs <*> [a]

just :: (a -> b) -> (a -> [b])
just f = \a -> [f a]

invertLs :: Int -> ([a] -> [a])
invertLs axis = 
    let ind = axis * 2
    in  swapLs ind (ind+1)

swapLs :: Int -> Int -> ([a] -> [a])
swapLs ai bi = \l -> setLs bi (l !! ai) $ setLs ai (l !! bi) l

invert :: Int -> (Tile3 -> Tile3)
invert i = 
    let invF = invertLs i
    in \t -> Tile3 {
        weightt         = weightt t,
        edgeQualifiers  = invF $ edgeQualifiers t,
        sides           = invF $ sides t,
        rots            = rots t,
        inverts         = modifyLs i not $ inverts t
    }

rotateX90 :: (Tile3 -> Tile3)
rotateX90 = \t -> Tile3 {
    weightt         = weightt t,
    edgeQualifiers  = rotateX90ls $ edgeQualifiers t,
    sides           = rotateX90ls $ sides t,
    rots            = modifyLs 0 (+90) $ rots t,
    inverts         = inverts t
}

rotateY90 :: (Tile3 -> Tile3)
rotateY90 = \t -> Tile3 {
    weightt         = weightt t,
    edgeQualifiers  = rotateY90ls $ edgeQualifiers t,
    sides           = rotateY90ls $ sides t,
    rots            = modifyLs 1 (+90) $ rots t,
    inverts         = inverts t
}

rotateZ90 :: (Tile3 -> Tile3)
rotateZ90 = \t -> Tile3 {
    weightt         = weightt t,
    edgeQualifiers  = rotateZ90ls $ edgeQualifiers t,
    sides           = rotateZ90ls $ sides t,
    rots            = modifyLs 2 (+90) $ rots t,
    inverts         = inverts t
}

-- [a, f(a), f(f(a)), f(f(f(a))), ...]
collectR :: (a -> a) -> Int -> (a -> [a])
collectR f n = \t -> (Prelude.take n $ iterate f t)

indexLoop :: (Int -> a -> b) -> Int -> [a] -> [b]
indexLoop f _ [] = []
indexLoop f i (a:as) = f i a : indexLoop f (i+1) as

setLs :: Int -> a -> ([a] -> [a])
setLs i v = \l -> (Prelude.take i l) ++ [v] ++ (Prelude.drop (i+1) l)

remLs :: Int -> ([a] -> [a])
remLs i = \l -> (Prelude.take i l) ++ (Prelude.drop (i+1) l)

modifyLs :: Int -> (a -> a) -> ([a] -> [a])
modifyLs i f = \l -> setLs i (f $ l !! i) l

----------------------------------------------------------------------

doSpinX = collectR rotateX90 4
doSpinY = collectR rotateY90 4
doSpinZ = collectR rotateZ90 4

doSpinXns = Prelude.drop 1 <$> collectR rotateX90 4
doSpinYns = Prelude.drop 1 <$> collectR rotateY90 4
doSpinZns = Prelude.drop 1 <$> collectR rotateZ90 4

doMirrorX = andSame $ just $ invert 0
doMirrorY = andSame $ just $ invert 1
doMirrorZ = andSame $ just $ invert 2

----------------------------------------------------------------------

allTrue :: [Bool]
allTrue = replicate 6 True

falsifyI :: [Bool] -> Int -> [Bool]
falsifyI ls i = setLs i False ls

falsifyIs :: [Bool] -> [Int] -> [Bool]
falsifyIs ls (i:is) = falsifyIs (falsifyI ls i) is
falsifyIs ls [] = ls

shapeEdges :: [[Bool]]
shapeEdges = [
    [True,True,True,True,True,True],[False,True,True,True,True,True],
    [True,False,True,True,True,True],[True,True,False,True,True,True],
    [True,True,True,False,True,True],[True,True,True,True,False,True],
    [True,True,True,True,True,False],[False,True,False,True,True,True],
    [False,True,True,False,True,True],[False,True,True,True,False,True],
    [False,True,True,True,True,False],[True,False,False,True,True,True],
    [True,False,True,False,True,True],[True,False,True,True,False,True],
    [True,False,True,True,True,False],[True,True,False,True,False,True],
    [True,True,False,True,True,False],[True,True,True,False,False,True],
    [True,True,True,False,True,False],[False,True,False,True,False,True],
    [False,True,False,True,True,False],[False,True,True,False,False,True],
    [False,True,True,False,True,False],[True,False,False,True,False,True],
    [True,False,False,True,True,False],[True,False,True,False,False,True],
    [True,False,True,False,True,False]]
-- shapeEdges = nub ([[True, True, True, True, True, True]]
--     ++ (falsifyI allTrue <$> [0..5])
--     ++ [falsifyIs allTrue [axis*2+sign, axes]
--         | axis <- [0..2]
--         , sign <- [0, 1]
--         , axes <- [n*2+sign2 | n <- (remLs axis [0..2]), sign2 <- [0,1]]]
--     ++ [falsifyIs allTrue [0+xs, 2+ys, 4+zs]
--         | xs <- [0,1]
--         , ys <- [0,1]
--         , zs <- [0,1]])

----------------------------------------------------------------------

matcher :: [Tile3] -> [Tile]
matcher t3s = indexLoop f 0 t3s where
    f = \i t3 -> 
        let validShapes = [si | si<-[0..(length shapeEdges)-1], all id $ (zipWith edgeQualifies (edgeQualifiers t3) (shapeEdges !! si))]
        in Tile {
            weight = weightt t3,
            fitsInto = \s -> elem s validShapes,
            allowedNeighbors = Map.fromList $ [
                ([-1,1,-2,2,-3,3] !! dir, Set.fromList [otherTi
                    | otherTi <- [0..(length t3s)-1]
                    , not $ disjoint ((sides t3) !! dir) ((sides $ t3s !! otherTi) !! opp)])
                | (dir, opp) <- [(0,1), (1,0), (2,3), (3,2), (4,5), (5,4)]
            ]
        }