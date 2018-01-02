# Register-Allocation
Implemented as a graph coloring algorithm in OCaml for compiler optimization.

Based on Graph Theory, graph coloring assigns colors to each node such that no two adjacent nodes share the same color. The algorithm was designed to allocate space (registers) to variables in a program, but is written so it can be generalized to other contexts as well. 

In the context of space allocation each unit of space (register) is a color and each variable is a node. Two variables share an edge if their values are live at the same time and cannot be assigned the same register.
