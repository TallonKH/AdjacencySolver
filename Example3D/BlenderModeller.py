import bpy
import math
import json
from dataclasses import dataclass
from typing import List

gridPath = "/Users/default/Google Drive/Personal Stuff/Code/AdjacencySolver2/Example3D/grid.txt"
palettePath = "/Users/default/Google Drive/Personal Stuff/Code/AdjacencySolver2/Example3D/palette.txt"

# scene = bpy.data.scenes[0]
parts = bpy.data.collections['Parts'].objects
destination = bpy.data.collections['Output']
scale = 2

@dataclass
class Tile:
    name: str
    rots: List[int]
    flips: List[bool]

def main():
    clearDest()
    with open(palettePath, "r") as tilesf, open(gridPath, "r") as gridf:
        tiles = []
        line = tilesf.readline()
        while line:
            vals = line.split(" ")
            tiles.append(
                Tile(
                    name=vals[0],
                    rots=[int(v) for v in vals[1:4]],
                    flips=[v == "True" for v in vals[4:7]]
                )
            )

            line = tilesf.readline()

        grid = json.loads(gridf.read())
        grid = [grid[n*8:(n+1)*8] for n in range(64)]
        grid = [grid[n*8:(n+1)*8] for n in range(8)]

        z = 0
        for rect in grid:
            y = 0
            for line in rect:
                x = 0
                for tileI in line:
                    tile = tiles[tileI]
                    if(tile.name != "Null"):
                        part = parts[tile.name]
                        copy = dupe(part)
                        copy.location = (x*scale, y*scale, z*scale)
                        copy.scale = (btf(tile.flips[0]), btf(tile.flips[1]), btf(tile.flips[2]))
                        copy.rotation_euler = (
                            rads(tile.rots[0]), rads(tile.rots[1]), rads(tile.rots[2])
                        )
                    x += 1
                y += 1
            z += 1


def rads(degs):
    return degs / 180 * math.pi

def dupe(src):
    cop = src.copy()
    # cop.data = src.data.copy()
    destination.objects.link(cop)
    return cop

def btf(bol):
    return -1 if bol else 1

def clearDest():
    for obj in destination.objects:
        bpy.data.objects.remove(obj, do_unlink=True)

main()
