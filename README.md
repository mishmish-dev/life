# life

```
$ ./life examples/marching_gliders.cells

  Size: 14 x 12
  Generation: 139
┌─────────────────────────────┐
│                             │
│               □             │
│                 □ □         │
│               □ □           │
│                             │
│   □                         │
│     □ □         □           │
│   □ □             □ □       │
│                 □ □         │
│                             │
│                             │
│                             │
└─────────────────────────────┘
```

An implementation of Conway's Game of Life with POSIX terminal interface. The grid is toroidal, i.e. coordinates wrap around the edges.

The program reads an initial state of the world from a file provided in cmdline argument or from the standard input. 

## Input format

Input for the program looks like this and follows the plaintext pattern format from [Game of Life Wiki](https://conwaylife.com/):

```
! COMMENT
.O.....
..O....
OOO....
.......
.......
```

`.` represents dead cell, `O` represents alive cell, and rows are separated by newlines, and characters after `!` until the end of line are ignored.

Check out `examples` folder for reference.

## Building and running

The project is configured using Stack, so you can use either it or Cabal

```shell
# cabal
cabal run life -- examples/marching_gliders.txt

# stack
stack run -- examples/marching_gliders.txt
```
