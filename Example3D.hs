module Grid3Example where

import AdjacencySolver
import Data.Set as Set
import GridGen3

air = Tile3 {
    weightt = 1,
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
    weightt = 1,
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
    weightt = 1,
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
pipeTs = [pipeElbow] >>= doSpinZ >>= collect [same, just rotateX90, just (rotateX90 . rotateZ90)]

grid = generateGrid3 False 8 8 8

pal = Palette $ matcher $ air : pipeIs ++ pipeElbows ++ pipeTs