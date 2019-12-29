from dataclasses import dataclass, field
from pymonad.List import *
from typing import List as TList
from pymonad.Reader import curry
# axes are 0 1 2
# dirs are 0, 1, 2, -3, -2, -1
dirNames = ["forward", "right", "up", "down", "left", "backward"]


@dataclass
class TTile:
    sides: TList[int]
    scale: TList[int] = field(default_factory=lambda: [1, 1, 1])
    rotat: TList[int] = field(default_factory=lambda: [0, 0, 0])


def opposite(dir):
    return -dir-1

# def rearrange(til, newOrder):
#     return [til[newOrder[i]] for i in range(len(til))]


def copy(tt):
    return TTile(
        sides=list(tt.sides),
        scale=list(tt.scale),
        rotat=list(tt.rotat)
    )


def tInvertScale(axis, til):
    cop = copy(til)
    cop.scale[axis] *= -1
    return cop


@curry
def tRot90(axis, til):
    cop = copy(til)
    cop.rotat[axis] += 90
    return cop


@curry
def rotX90(til):
    return chain([swapAxes(1, 2), mirror(1)])(tRot90(0, til))


@curry
def mirror(axis, til):
    return List(swap(axis, opposite(axis), til))


@curry
def tMirror(axis, til):
    return List(
        tInvertScale(axis, swap(axis, opposite(axis), til))
    )


def swap(side1, side2, til):
    swp = copy(til)
    swp.sides[side1] = til.sides[side2]
    swp.sides[side2] = til.sides[side1]
    return swp


@curry
def swapAxes(axis1, axis2, til):
    return List(swap(axis1, axis2, til))


@curry
def andOriginal(f, til):
    return List(til) + f(til)


mirX = andOriginal(tMirror(0))
mirY = andOriginal(tMirror(1))
mirZ = andOriginal(tMirror(2))


@curry
def same(til):
    return List(til)


@curry
def collect(args, til):
    return List(*(arg(til) for arg in args))


oneAxisMirrors = collect([same, mirror(0), mirror(1), mirror(2)])


@curry
def chain(args, til):
    if(len(args) == 0):
        return List(til)
    fst = args.pop(0)
    return fst(til) >> chain(args)


# rotation guide
# first part                                  second part
# nothing                                   & nothing
# nothing                                   & flip both axes
# flip axes & mirror on one of the axes     & nothing
# flip axes & mirror on one of the axes     & flip both axes
def rotX(til): 
    return andOriginal(rotX90)(til) >> andOriginal(chain([tMirror(1), tMirror(2)]))
def rotY(til): 
    return andOriginal(chain([swapAxes(0, 2), mirror(2), rot90(1)]))(
til) >> andOriginal(chain([tMirror(0), tMirror(2)]))
def rotZ(til): return andOriginal(chain([swapAxes(0, 1), mirror(0), rot90(2)]))(
    til) >> andOriginal(chain([tMirror(0), tMirror(1)]))
# !!! if registering rotations, don't use both mirrors of the other axis
# rotZ(180) will collide with (rotX >> rotY)


# to generate all permutations for totally non-symmetrical shape, do oneAxisMirrors >> rotX >> rotY >> rotZ
# print(len(List(["Xp", "Yp", "Zp", "Zn", "Yn", "Xn"]) >> rotX >> rotY >> rotZ >> oneAxisMirrors))
# print(List(TTile(["Xp", "Yp", "Zp", "Zn", "Yn", "Xn"])) >> andOriginal(rotX90))
# print(List(TTile(["Xp", "Yp", "Zp", "Zn", "Yn", "Xn"])) >> andOriginal(tMirror(0)) >>
#       andOriginal(lambda tis : tMirror(1,tis) >> tMirror(2)))
print(List(TTile(["Xp", "Yp", "Zp", "Zn", "Yn", "Xn"])) >> andOriginal(
    chain([tMirror(0)])) >> andOriginal(chain([tMirror(1)])))
