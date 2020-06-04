# mazer-hs
This project is to replace [OliverMead/mazer](https://github.com/OliverMead/mazer),
a project I started in C.
I believe that functional programming is better suited to this task, so I am
using Haskell.
Update: this code is not good Haskell and should not be used for reference, I will replace it eventually.
## Building
This project uses the [stack](https://docs.haskellstack.org/en/stable/build_command/) build system.
## Implementation
The algorithm used here checks every node to see if it is a dead end based on a traveling-from node.
This is a much simpler algorithm for mazes than, for example, A\* or Dijkstra's.
The algorithm here has been improved by only checking nodes if they are a 
branching point and not simply a path between branching points.

Previously, each point in the input data has either represented a wall or a path. Now each point, 
named a cell, is a path that may contain any combination of walls in the four usable directions.

With this new implementation, each cell is now displayed using Unicode box drawing characters (e.g. ╋). When 
displaying dead ends, the cell is represented by a single lined equivalent.

Looping sections of maze have been allowed simply by storing a list of already checked nodes
which must not be checked again, to avoid recursion without an exit point.
### Defining a 'Dead' end 
In the context of this program, a dead end is defined as a set of nodes from which, without going back 
to the previous node, an exit cannot be reached. 

With this definition, looping sections, no matter their location in the maze, are not removed as a dead 
end in finding the solution to the maze. I made this decision since loops create multiple paths to the 
exit of the maze and to only show the shortest solution may not be desirable.

## What Works?
- Generating a maze from a 2D list of MazeCells (integers representing the directions of up to 4 walls)
- Removing dead-end nodes from the generated list
- Automatically finding an entry-point
- Displaying the 'solved' maze based on the difference between the new list of nodes and the original list
  of nodes
- Only check nodes that aren't 'paths'
- Automatically find the size of the maze
## To Do
- Generate random, large mazes to solve
- (Potentially) implement export to bitmap functionality, for generated and solved 
- (Potentially) implement bitmap reading to load and solve B/W image based mazes
