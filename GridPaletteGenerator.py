from pymonad.List import *
from pymonad.Reader import curry
# axes are 0 1 2
# dirs are 0, 1, 2, -3, -2, -1
dirNames = ["forward", "right", "up", "down", "left", "backward"]


def opposite(dir):
    return -dir-1

def rearrange(til, newOrder):
    return [til[newOrder[i]] for i in range(len(til))]

def copy(ls):
    return list(ls)

@curry
def mirror(axis, til):
    return List(swap(axis, opposite(axis), til))

def swap(side1, side2, til):
    swp = copy(til)
    swp[side1] = til[side2]
    swp[side2] = til[side1]
    return swp

@curry
def swapAxes(axis1, axis2, til):
    return List(swap(axis1, axis2, til))

@curry
def andOriginal(f, til):
    return List(
        til,
        f(til)
    )

def oneAxisMirrors(til):
    return List(
        til,
        swap(0, -1, til),
        swap(1, -2, til),
        swap(2, -3, til)
    )

mirX = andOriginal(mirror(0))
mirY = andOriginal(mirror(1))
mirZ = andOriginal(mirror(2))

@curry
def chain(args, til):
    if(len(args) == 0):
        return til
    fst = args.pop(0)
    return fst(til) >> chain(args)

# rotation guide
# first part                                  second part
# nothing                                   & nothing
# nothing                                   & flip both axes
# flip axes & mirror on one of the axes     & nothing
# flip axes & mirror on one of the axes     & flip both axes
rotX = lambda til : andOriginal(chain([swapAxes(1, 2), mirror(1)]))(til) >> andOriginal(chain([mirror(1),mirror(2)]))
rotY = lambda til : andOriginal(chain([swapAxes(0, 2), mirror(0)]))(til) >> andOriginal(chain([mirror(0),mirror(2)]))
rotZ = lambda til : andOriginal(chain([swapAxes(0, 1), mirror(0)]))(til) >> andOriginal(chain([mirror(0),mirror(1)]))
# !!! if registering rotations, don't use both mirrors of the other axis
# rotZ(180) will collide with (rotX >> rotY)

# to generate all permutations, do oneAxisMirrors >> rotX >> rotY >> rotZ
print(len(List(["Xp", "Yp", "Zp", "Zn", "Yn", "Xn"]) >> rotX >> rotY >> rotZ >> oneAxisMirrors))
