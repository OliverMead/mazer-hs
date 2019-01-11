# mazer-hs
This project is to replace [OliverMead/mazer](https://github.com/OliverMead/mazer),
a project I started in C.
I believe that functional programming is better suited to this task, so I am
using Haskell.
## Building
This project uses the [stack](https://docs.haskellstack.org/en/stable/build_command/) build system.
## Implementation
The algorithm used here checks every node to see if it is a dead end based on a traveling-from node.
This is a much simpler algorithm for mazes than, for example, A\* or Dijkstra's.
In terms of efficiency, the algorithm here has been improved by only checking nodes if they are a 
branching point and not simply a path between branching points.

Looping sections of maze have been allowed simply by storing a list of already checked nodes
which must not be checked again, to avoid recursion without an exit point.
## What Works?
- Generating a maze from a 2D list of 1s for walls and 0s for paths (by creating a list of path nodes)
- Removing dead-end nodes from the generated list
- Automatically finding an entry-point
- Displaying the 'solved' maze based on the difference between the new list of nodes and the original list of nodes
- Only check nodes that aren't 'paths'
- Automatically find the size of the maze
## To Do
- Generate random, large mazes to solve
- (Potentially) implement export to bitmap functionality, for generated and solved 
- (Potentially) implement bitmap reading to load and solve B/W image based mazes
