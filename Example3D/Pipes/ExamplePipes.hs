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
        Set.fromList ["air"],
        Set.fromList ["pipe"],
        Set.fromList ["pipe"],
        Set.fromList ["air"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

pipeIs = collect [same, just rotateY90, just rotateZ90] pipeI
pipeElbows = collect [same, \a -> [a] >>= doSpinXns >>= doMirrorX, \a -> [a] >>= doSpinYns >>= doMirrorY] pipeElbow
pipeTs = collect [
    \s -> [s, rotateX90 s] >>= (\s1 -> [s1, invert 0 s]), 
    \s -> [rotateZ90 s] >>= (\s1 -> [s1, rotateY90 s1]) >>= (\s2 -> [s2, invert 1 s]),
    \s -> [rotateY90 s] >>= (\s1 -> [s1, rotateZ90 s1]) >>= (\s2 -> [s2, invert 2 s])] pipeT

-- t3s = air : pipeIs ++ pipeElbows ++ pipeTs
t3s = air : pipeIs ++ pipeElbows ++ pipeTs

grid = generateGrid3 False 8 8 8

pal = Palette $ matcher t3s