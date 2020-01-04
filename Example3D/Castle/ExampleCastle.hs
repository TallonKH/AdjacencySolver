module Grid3Example where

import AdjacencySolver
import Data.Set as Set
import GridGen3

air = Tile3 {
    name = "Null",
    weightt = 50,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
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

grassWild = Tile3 {
    name = "GrassWild",
    weightt = 30,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["dirt"],
        Set.fromList ["air", "grass_wild"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

grassInner = Tile3 {
    name = "GrassInner",
    weightt = 20,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["dirt"],
        Set.fromList ["air", "grass_inner"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

spireBaseL = Tile3 {
    name = "Spire_Base_L",
    weightt = 0.1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["dirt"],
        Set.fromList ["spire"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

spireBaseR = Tile3 {
    name = "Spire_Base_R",
    weightt = 0.1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["dirt"],
        Set.fromList ["spire"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

wallBaseL = Tile3 {
    name = "Wall_Base_L",
    weightt = 1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_wild"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["dirt"],
        Set.fromList ["wall"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

wallBaseR = Tile3 {
    name = "Wall_Base_R",
    weightt = 1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_inner"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["dirt"],
        Set.fromList ["wall"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBaseL = Tile3 {
    name = "Sentry_Base_L",
    weightt = 0.2,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_wild"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["dirt"],
        Set.fromList ["sentry"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBaseR = Tile3 {
    name = "Sentry_Base_R",
    weightt = 0.2,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_inner"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["dirt"],
        Set.fromList ["sentry"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBendBaseL1 = Tile3 {
    name = "Sentry_Bend_Base_L1",
    weightt = 0.05,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["wallBaseL"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["dirt"],
        Set.fromList ["sentryBend"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBendBaseR1 = Tile3 {
    name = "Sentry_Bend_Base_R1",
    weightt = 0.05,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["wallBaseR"],
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["dirt"],
        Set.fromList ["sentryBend"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}
sentryBendBaseL2 = Tile3 {
    name = "Sentry_Bend_Base_L2",
    weightt = 0.05,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["grass_wild"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["grass_wild"],
        Set.fromList ["dirt"],
        Set.fromList ["sentryBend"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBendBaseR2 = Tile3 {
    name = "Sentry_Bend_Base_R2",
    weightt = 0.05,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["grass_inner"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["grass_inner"],
        Set.fromList ["dirt"],
        Set.fromList ["sentryBend"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

spireTop = Tile3 {
    name = "Spire_Top",
    weightt = 1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["spire"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

spireBody = Tile3 {
    name = "Spire_Body",
    weightt = 0.2,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["spire"],
        Set.fromList ["spire"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

wallTop = Tile3 {
    name = "Wall_Top",
    weightt = 0.2,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallTop"],
        Set.fromList ["wallTop"],
        Set.fromList ["wall"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryTop = Tile3 {
    name = "Sentry_Top",
    weightt = 0.2,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallTop"],
        Set.fromList ["wallTop"],
        Set.fromList ["sentry"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBendTop = Tile3 {
    name = "Sentry_Bend_Top",
    weightt = 0.01,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        Set.fromList ["wallTop"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallTop"],
        Set.fromList ["sentryBend"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

t3s = [air, grassWild, grassInner, spireBaseL, spireBaseR, spireTop, spireBody]
    ++ ([
        wallBaseL, wallBaseR, sentryBaseL, sentryBaseR, 
        sentryBendBaseR1,sentryBendBaseR2, sentryBendBaseL1, sentryBendBaseL2, 
        wallTop, sentryTop] >>= andSame (just rotateZ90))
    ++ ([sentryBendTop] >>= doSpinZ)

grid = generateGrid3 False 8 8 4

pal = Palette $ matcher t3s

runner = runOn (intersectOptions 0 (Set.singleton 1) =<< (newSolution grid pal))