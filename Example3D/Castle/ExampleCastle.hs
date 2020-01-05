module Grid3Example where

import AdjacencySolver
import Data.Set as Set
import GridGen3

air = Tile3 {
    name = "Null",
    weightt = 50,
    edgeQualifiers = [Any, Any, Any, Any, Neighboring, Any],
    sides = ["air", "air", "air", "air", "air", "air"],
    otherSides = [
        Set.fromList ["air", "wallSide"],
        Set.fromList ["air", "wallSide"],
        Set.fromList ["air", "wallSide"],
        Set.fromList ["air", "wallSide"],
        Set.fromList ["air", "grass_wild", "grass_inner", "wallSide", "path"],
        Set.fromList ["air", "wallSide"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

grassWild = Tile3 {
    name = "GrassWild",
    weightt = 30,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        "grass_wild", "grass_wild", 
        "grass_wild", "grass_wild", 
        "dirt", "grass_wild"],
    otherSides = [
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["dirt"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

grassInner = Tile3 {
    name = "GrassInner",
    weightt = 20,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        "grass_inner", "grass_inner", 
        "grass_inner", "grass_inner", 
        "dirt", "grass_inner"],
    otherSides = [
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["dirt"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

spireBaseW = Tile3 {
    name = "Spire_Base_W",
    weightt = 0.1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Neighboring],
    sides = [
        "grass_wild", "grass_wild",
        "grass_wild", "grass_wild",
        "dirt","spire"],
    otherSides = [
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["dirt"],
        Set.fromList ["spire"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

spireBaseI = Tile3 {
    name = "Spire_Base_I",
    weightt = 0.1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Neighboring],
    sides = [
        "grass_inner", "grass_inner",
        "grass_inner", "grass_inner",
        "dirt","spire"],
    otherSides = [
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["dirt"],
        Set.fromList ["spire"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

wallBase = Tile3 {
    name = "Wall_Base",
    weightt = 2,
    edgeQualifiers = [Any, Any, Neighboring, Neighboring, Any, Neighboring],
    sides = [
        "grass_inner", "grass_wild",
        "wallBaseL", "wallBaseR",
        "dirt", "wall"
    ],
    otherSides = [
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_wild"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["dirt"],
        Set.fromList ["wall"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

gateBase = Tile3 {
    name = "Gate_Base",
    weightt = 0.05,
    edgeQualifiers = [Any, Any, Neighboring, Neighboring, Any, Neighboring],
    sides = [
        "grass_inner", "path",
        "wallBaseL", "wallBaseR",
        "dirt", "gate"
    ],
    otherSides = [
        Set.fromList ["grass_inner"],
        Set.fromList ["path"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["dirt"],
        Set.fromList ["gate"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBase = Tile3 {
    name = "Sentry_Base",
    weightt = 0.05,
    edgeQualifiers = [Any, Any, Neighboring, Neighboring, Any, Neighboring],
    sides = [
        "grass_inner", "grass_wild",
        "wallBaseL", "wallBaseR",
        "dirt", "sentry"
    ],
    otherSides = [
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_wild"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["dirt"],
        Set.fromList ["sentry"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBendBaseW = Tile3 {
    name = "Sentry_Bend_Base_W",
    weightt = 0.02,
    edgeQualifiers = [Neighboring, Any, Any, Neighboring, Any, Neighboring],
    sides = [
        "wallBaseL", "grass_wild", 
        "grass_wild", "wallBaseR",
        "dirt", "sentryBend"
    ],
    otherSides = [
        Set.fromList ["wallBaseR"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["wallBaseL"],
        Set.fromList ["dirt"],
        Set.fromList ["sentryBend"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBendBaseI = Tile3 {
    name = "Sentry_Bend_Base_I",
    weightt = 0.01,
    edgeQualifiers = [Neighboring, Any, Any, Neighboring, Any, Neighboring],
    sides = [
        "wallBaseR", "grass_inner", 
        "grass_inner", "wallBaseL",
        "dirt", "sentryBend"
    ],
    otherSides = [
        Set.fromList ["wallBaseL"],
        Set.fromList ["grass_inner"],
        Set.fromList ["grass_inner"],
        Set.fromList ["wallBaseR"],
        Set.fromList ["dirt"],
        Set.fromList ["sentryBend"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

spireTop = Tile3 {
    name = "Spire_Top",
    weightt = 1,
    edgeQualifiers = [Any, Any, Any, Any, Neighboring, Any],
    sides = [
        "wallSide", "wallSide",
        "wallSide", "wallSide",
        "spire", "air"],
    otherSides = [
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
    edgeQualifiers = [Any, Any, Any, Any, Neighboring, Neighboring],
    sides = [
        "wallSide", "wallSide",
        "wallSide", "wallSide",
        "spire", "spire"],
    otherSides = [
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
    edgeQualifiers = [Any, Any, Neighboring, Neighboring, Neighboring, Any],
     sides = [
        "wallSide", "wallSide",
        "wallTop", "wallTop",
        "wall", "air"],
    otherSides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallTop"],
        Set.fromList ["wallTop"],
        Set.fromList ["wall"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

gateTop = Tile3 {
    name = "Gate_Top",
    weightt = 0.2,
    edgeQualifiers = [Any, Any, Neighboring, Neighboring, Neighboring, Any],
     sides = [
        "wallSide", "wallSide",
        "wallTop", "wallTop",
        "gate", "air"],
    otherSides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallTop"],
        Set.fromList ["wallTop"],
        Set.fromList ["gate"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

wallBody = Tile3 {
    name = "Wall_Body",
    weightt = 0.05,
    edgeQualifiers = [Any, Any, Neighboring, Neighboring, Neighboring, Neighboring],
    sides = [
        "wallSide", "wallSide",
        "wallMid", "wallMid",
        "wall", "wall"],
    otherSides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallMid"],
        Set.fromList ["wallMid"],
        Set.fromList ["wall"],
        Set.fromList ["wall"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryTop = Tile3 {
    name = "Sentry_Top",
    weightt = 0.01,
    edgeQualifiers = [Any, Any, Neighboring, Neighboring, Neighboring, Any],
    sides = [
        "wallSide", "wallSide",
        "wallTop", "wallTop",
        "sentry", "air"],
    otherSides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallTop"],
        Set.fromList ["wallTop"],
        Set.fromList ["sentry"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryTall = Tile3 {
    name = "Sentry_Tall",
    weightt = 0.01,
    edgeQualifiers = [Any, Any, Neighboring, Neighboring, Neighboring, Neighboring],
    sides = [
        "wallSide", "wallSide",
        "wallTop", "wallTop",
        "sentry", "spire"],
    otherSides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallTop"],
        Set.fromList ["wallTop"],
        Set.fromList ["sentry"],
        Set.fromList ["spire"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBody = Tile3 {
    name = "Sentry_Body",
    weightt = 0.05,
    edgeQualifiers = [Any, Any, Neighboring, Neighboring, Neighboring, Neighboring],
    sides = [
        "wallSide", "wallSide",
        "wallMid", "wallMid",
        "sentry", "sentry"],
    otherSides = [
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallMid"],
        Set.fromList ["wallMid"],
        Set.fromList ["sentry"],
        Set.fromList ["sentry"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBendTop = Tile3 {
    name = "Sentry_Bend_Top",
    weightt = 0.005,
    edgeQualifiers = [Neighboring, Any, Any, Neighboring, Neighboring, Any],
    sides = [
        "wallTop", "wallSide",
        "wallSide", "wallTop",
        "sentryBend", "air"],
    otherSides = [
        Set.fromList ["wallTop"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallTop"],
        Set.fromList ["sentryBend"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBendTall = Tile3 {
    name = "Sentry_Bend_Tall",
    weightt = 0.003,
    edgeQualifiers = [Neighboring, Any, Any, Neighboring, Neighboring, Neighboring],
    sides = [
        "wallTop", "wallSide",
        "wallSide", "wallTop",
        "sentryBend", "spire"],
    otherSides = [
        Set.fromList ["wallTop"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallTop"],
        Set.fromList ["sentryBend"],
        Set.fromList ["spire"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

sentryBendBody = Tile3 {
    name = "Sentry_Bend_Body",
    weightt = 0.002,
    edgeQualifiers = [Neighboring, Any, Any, Neighboring, Neighboring, Neighboring],
    sides = [
        "wallMid", "wallSide",
        "wallSide", "wallMid",
        "sentryBend", "sentryBend"],
    otherSides = [
        Set.fromList ["wallMid"],
        Set.fromList ["air"],
        Set.fromList ["air"],
        Set.fromList ["wallMid"],
        Set.fromList ["sentryBend"],
        Set.fromList ["sentryBend"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

path = Tile3 {
    name = "Path",
    weightt = 1,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        "grass_wild", "grass_wild",
        "path", "path",
        "dirt", "path"],
    otherSides = [
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["path"],
        Set.fromList ["path"],
        Set.fromList ["dirt"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

pathBend = Tile3 {
    name = "Path_Bend",
    weightt = 0.3,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        "path", "grass_wild",
        "grass_wild", "path",
        "dirt", "path"],
    otherSides = [
        Set.fromList ["path"],
        Set.fromList ["grass_wild"],
        Set.fromList ["grass_wild"],
        Set.fromList ["path"],
        Set.fromList ["dirt"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

pathT = Tile3 {
    name = "Path_T",
    weightt = 0.01,
    edgeQualifiers = [Any, Any, Any, Any, Any, Any],
    sides = [
        "path", "grass_wild",
        "path", "path",
        "dirt", "path"],
    otherSides = [
        Set.fromList ["path"],
        Set.fromList ["grass_wild"],
        Set.fromList ["path"],
        Set.fromList ["path"],
        Set.fromList ["dirt"],
        Set.fromList ["air"]],
    rots = [0,0,0],
    inverts = [False, False, False]
}

t3s = [air, grassWild, grassInner, spireBaseW, spireBaseI, spireTop, spireBody]
    ++ ([wallTop, wallBody, sentryTop, sentryBody, sentryTall, gateTop] >>= andSame (just rotateZ90))
    ++ ([sentryBendBaseW, sentryBendBaseI, sentryBendTop, sentryBendTall, sentryBendBody, 
    wallBase, sentryBase, gateBase, path, pathBend, pathT] >>= doSpinZ)

grid = generateGrid3 False 16 16 4

pal = Palette $ matcher t3s

runner = runOn (intersectOptions 0 (Set.singleton 1) =<< (newSolution grid pal))