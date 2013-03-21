
1. error reporting
    - simple error messages
    - nested messages -- i.e. stack of errors
    - complex error info:  rest of token stream
    - complex error info:  position
2. change underlying monad
    - Maybe: deterministic, prioritized
    - List: non-deterministic, depth-first
    - Logic: non-deterministic, breadth-first
3. partial results (can also work for monadic parsers)
    - global log
    - 'local' log
    - multiple writers for different types
    - algebraic data type for different types
4. brace matching
5. miscellaneous
    - could also show how to 'observe' backtracking using logging


also, can demonstrate how:
 1) individual parsers can work with multiple different stacks
 2) parsers can work with different stack orders, even when their
    signature says they work with 2+ different transformers, although
    this is probably more of a bug than a feature