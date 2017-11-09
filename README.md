# Peace, Death! safe unlocker

Unlocks this puzzle: https://www.youtube.com/watch?v=TlE9gYDNUGI

# Method

It find a solution by simple BFS. There are patterns that can never be reached from some input, which make the solver falls into infinite loop.
My friend, who is know well about puzzles, introduced an idea that a rotation can be composed of two elementary swap operation. 
And it is solvable only if number of elementary swap operation is multiple of 2. This is why ```is_transformable``` got called before ```solution``` got called.
