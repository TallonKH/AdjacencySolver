module Grid2Example where

import GridGen

testTile0 = Tile {
    weight = 10,
    fitsInto = const True,
    allowedNeighbors = Map.fromList [
        (-1, Set.fromList [0,1,3]),
        (1, Set.fromList [0,1,3]),
        (-2, Set.fromList [0,1,2]),
        (2, Set.fromList [0,1,2])]
}
testTile1 = Tile {
    weight = 1,
    fitsInto = const True,
    allowedNeighbors = Map.fromList [
        (-1, Set.fromList [0,2]),
        (1, Set.fromList [0,2]),
        (-2, Set.fromList [0,3]),
        (2, Set.fromList [0,3])]
}
testTile2 = Tile {
    weight = 5,
    fitsInto = const True,
    allowedNeighbors = Map.fromList [
        (-1, Set.fromList [1,2]),
        (1, Set.fromList [1,2]),
        (-2, Set.fromList [0]),
        (2, Set.fromList [0])]
}
testTile3 = Tile {
    weight = 5,
    fitsInto = const True,
    allowedNeighbors = Map.fromList [
        (-1, Set.fromList [0]),
        (1, Set.fromList [0]),
        (-2, Set.fromList [1,3]),
        (2, Set.fromList [1,3])]
}

testPal = Palette [testTile0, testTile1, testTile2, testTile3]
testGrid = generateGrid2 True 32 32
testSol = newSolution testGrid testPal
