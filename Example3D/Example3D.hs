module Grid3Example where

import AdjacencySolver
import Data.Set as Set
import GridGen3

air = Tile3 {
    name = "Null",
    weightt = 10,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    -- edgeQualifiers  = [Neighboring,Neighboring,Neighboring,Neighboring,Neighboring,Neighboring],
    sides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

pipeI = Tile3 {
    name = "PipeI",
    weightt = 1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    -- edgeQualifiers  = [Neighboring,Neighboring,Neighboring,Neighboring,Neighboring,Neighboring],
    sides = [
        Set.fromList ["pipe"],
        Set.fromList ["pipe"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

pipeElbow = Tile3 {
    name = "PipeElbow",
    weightt = 0.1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    -- edgeQualifiers  = [Neighboring,Neighboring,Neighboring,Neighboring,Neighboring,Neighboring],
    sides = [
        Set.fromList ["pipe"],
        Set.fromList ["air"],
        Set.fromList ["pipe"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

pipeT = Tile3 {
    name = "PipeT",
    weightt = 0.1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    -- edgeQualifiers  = [Neighboring,Neighboring,Neighboring,Neighboring,Neighboring,Neighboring],
    sides = [
        Set.fromList ["pipe"],
        Set.fromList ["pipe"],
        Set.fromList ["pipe"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

pipeIs = collect [same, just rotateY90, just rotateZ90] pipeI
pipeElbows = collect [same, \a -> [a] >>= doSpinXns >>= doMirrorX, \a -> [a] >>= doSpinYns >>= doMirrorY] pipeElbow
-- pipeTs = [pipeT] >>= doSpinZ >>= collect [same, just rotateX90, just (rotateX90 . rotateZ90)]

-- t3s = air : pipeIs ++ pipeElbows ++ pipeTs
t3s = air : pipeIs ++ pipeElbows

grid = generateGrid3 False 8 8 8

pal = Palette $ matcher t3s