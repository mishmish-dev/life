# life

```
$ ./life examples/marching_gliders.txt

  Size: 14 x 12
  Generation: 80
┌─────────────────────────────┐
│                             │
│   #                         │
│     #           #           │
│ # # #             #         │
│               # # #         │
│                             │
│                             │
│                             │
│                             │
│               #             │
│                 #           │
│             # # #           │
└─────────────────────────────┘
```

An implementation of Conway's Game of Life with POSIX terminal interface. The grid is toroidal, i.e. coordinates wrap around the edges.

The program reads an initial state of the world from a file provided in cmdline argument or from the standard input. 

## Input format

Input for the program looks as simple as this:

```
.#.....
..#....
###....
.......
.......
```

`.` represents dead cell, `#` represents alive cell, and rows are separated by newlines.

Check out `examples` folder for reference.

## Building and running

The project is configured using Stack, so you can use either it or Cabal

```shell
# cabal
cabal run life -- examples/marching_gliders.txt

# stack
stack run -- examples/marching_gliders.txt
```
