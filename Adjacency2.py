# from sortedcontainers import SortedSet
# from pymonad import *
from dataclasses import dataclass
from typing import Dict, Tuple, List, Set, NewType, Callable
import random
import copy

Direction = NewType("Direction", str)
Shape = NewType("Shape", str)

# a type of tile that can occupy a slot
@dataclass
class Tile:
    index: int
    fitsInto: Callable[[Shape], bool]
    allowedNeighbors: Dict[Direction, Set[int]]


@dataclass
class Palette:
    tiles: List[Tile]

# unchanging - no info of reducedness
@dataclass
class Slot:
    index: int
    shape: Shape
    neighbors: Dict[Direction, int]


# unchanging board layout (description of the grid, with no info on reducedness)
@dataclass
class Board:
    slots: List[Slot]

# potential solution that satisfies a Board
@dataclass
class Solution:
    palette: Palette
    board: Board
    poptions: List[Set[int]]  # option set for each tile in a Board
    # set of indices of slots that aren't solved - not sorted because sorting gets confusing when option count changes
    unsolved: Set[int]


def newSolution(board: Board, palette: Palette):
    poptions = [set(ti for ti in range(len(palette.tiles)) if palette.tiles[ti].fitsInto(
        slot.shape)) for slot in board.slots]
    if any(len(slotT) == 0 for slotT in poptions):
        print("Failed to construct new board!")
        return

    unsolved = set(pi for pi in range(len(poptions)) if len(poptions[pi]) > 1)
    sol = Solution(
        palette=palette,
        board=board,
        poptions=poptions,
        unsolved=unsolved
    )
    return sol


def cloneSolution(sol):
    return Solution(
        palette=sol.palette,
        board=sol.board,
        poptions=copy.deepcopy(sol.poptions),
        unsolved=copy.copy(sol.unsolved)
    )


def cleanSolved(sol: Solution):
    return [next(iter(op)) for op in sol.poptions]

# if a solution doesn't work, backtrack
# very slow, but reliable
def solveRecurs(sol: Solution, depth:int=0):
    if(len(sol.unsolved) == 0):
        return sol

    # pi = min(sol.unsolved, key=lambda pi: len(sol.poptions[pi]))
    pi = next(iter(sol.unsolved))
    for ti in weightedShuffle(sol.poptions[pi]):
        newSol = cloneSolution(sol)
        
        result = intersectOptions(newSol, pi, {ti})
        if(result == False):
            continue

        fsol = solveRecurs(newSol, depth+1)
        if(fsol):
            return fsol

    return None

def solveIter(sol: Solution):
    pass

# solves without keeping track of previous states - just hope that it gets it right the first time around
# very fast, but theoretically unreliable
def solvePray(sol: Solution):
    while(len(sol.unsolved) != 0):
        pi = next(iter(sol.unsolved))
        ti = random.choice(list(sol.poptions[pi]))
        result = intersectOptions(sol, pi, {ti})
        if(result == False):
            print("oof")
            return None
    return sol

def weightedShuffle(ls):
    cop = list(ls)
    random.shuffle(cop)
    return cop

def intersectOptions(sol: Solution, index: int, newOptions: Set[int]):
    return subtractOptions(sol, index, sol.poptions[index] - newOptions)

def subtractOptions(sol: Solution, index: int, removedOptions: Set[int]):
    slot = sol.board.slots[index]
    
    
    # in case options removed were not present in the first place, and no change happens
    diff = sol.poptions[index] - removedOptions
    if(len(diff) == len(sol.poptions[index])):
        return True
    sol.poptions[index] = diff

    # for convenience
    options = sol.poptions[index]

    if(len(options) == 0):
        return False
    elif(len(options) == 1):
        sol.unsolved.remove(index)

    # maybe go in order of neighbors with fewest options?
    for direction in slot.neighbors.keys():
        currentNeighborOptions = set.union(
            *(sol.palette.tiles[ti].allowedNeighbors[direction] for ti in options))
        removedNeighborOptions = set.union(
            *(sol.palette.tiles[ti].allowedNeighbors[direction] for ti in removedOptions)) - currentNeighborOptions
        if(len(removedNeighborOptions) > 0):
            result = subtractOptions(
                sol, slot.neighbors[direction], removedNeighborOptions)
            if(result == False):
                return False

    # true = solution is valid (but not necessarily completed)
    return True


def makeGrid(width, height, looping=False):
    i = 0
    grid = []
    ls = []
    for x in range(width):
        col = []
        grid.append(col)
        for y in range(height):
            slot = Slot(
                index=i,
                shape="square",
                neighbors=dict()
            )
            i += 1

            col.append(slot)
            ls.append(slot)

    for x in range(width):
        for y in range(height):
            if (looping or y < height-1):
                grid[x][y].neighbors["up"] = grid[x][(y+1) % height].index
            if (looping or y > 1):
                grid[x][y].neighbors["down"] = grid[x][(y-1) % height].index
            if (looping or x < width-1):
                grid[x][y].neighbors["right"] = grid[(x+1) % width][y].index
            if (looping or x > 1):
                grid[x][y].neighbors["left"] = grid[(x-1) % width][y].index

    return Board(
        slots=ls
    )

def example():
    typeA = Tile(
        index=0,
        fitsInto=lambda a: True,
        allowedNeighbors={
            "up": {0,1,2,4,6},
            "down": {0,1,2,4,6},
            "left": {0,1,3,5,6},
            "right": {0,1,3,5,6}
        }
    )
    typeB = Tile(
        index=1,
        fitsInto=lambda a: True,
        allowedNeighbors={
            "up": {0,3},
            "down": {0,3},
            "left": {0,2},
            "right": {0,2}
        }
    )
    typeC = Tile(
        index=2,
        fitsInto=lambda a: True,
        allowedNeighbors={
            "up": {0},
            "down": {0},
            "left": {1,2},
            "right": {1,2}
        }
    )
    typeD = Tile(
        index=3,
        fitsInto=lambda a: True,
        allowedNeighbors={
            "up": {1,3},
            "down": {1,3},
            "left": {0},
            "right": {0}
        }
    )
    typeE = Tile(
        index=4,
        fitsInto=lambda a: True,
        allowedNeighbors={
            "up": {0},
            "down": {0},
            "left": {6,4},
            "right": {6,4}
        }
    )
    typeF = Tile(
        index=5,
        fitsInto=lambda a: True,
        allowedNeighbors={
            "up": {6,5},
            "down": {6,5},
            "left": {0},
            "right": {0}
        }
    )
    typeG = Tile(
        index=6,
        fitsInto=lambda a: True,
        allowedNeighbors={
            "up": {0, 5},
            "down": {0, 5},
            "left": {0, 4},
            "right": {0, 4}
        }
    )

    symbols = " o|-|-#"
    w = 25
    h = 25
    grid = makeGrid(w, h, True)
    hyp = newSolution(grid, Palette([typeA, typeB, typeC, typeD, typeE, typeF, typeG]))
    # cleaned = cleanSolved(solveRecurs(hyp))
    cleaned = cleanSolved(solvePray(hyp))
    
    for i in range(w):
        print(" ".join(symbols[i] for i in cleaned[i*h:(i+1)*h]))

example()