# life

An implementation of Conway's Game of Life. The Grid is toroidal, i.e. indices wrap around the edges.

## Running

The executable reads an initial state of the world from the standard input, so you probably want to run it like this:

```shell
cat glider.txt | ./life
```
