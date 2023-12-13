# auto-diff

Simple implementation of automatic differentiation by OCaml. Contains forward differential and reverse differential.

1. Define a "minimum language" for describing functions that need to be differentiated. Since it supports let binding, any computational graph can be expressed.

2. The function is compiled into a machine language, which has only three instructions: binary operators, unary functions, and reading variables from the environment. The result of the i-th instruction is always placed at address i.

3. Perform automatic differentiation based on this machine language program.

What can be improved:

1. Currently, the environment is represented by a list, and the time complexity of each "Read" instruction is O(n). If the variable names are compiled to integers and the environment is represented as a float array, this can be O(1).

2. Redesign the interface. Contains an abstract data structure to represent the functions of a minimal language. Provides a mechanism for combining these functions. With it, building multi-layer neural networks will be convenient.
