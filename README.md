# Elm Tracing Interpreter

![image](/img/fac-screenshot.png)


We love Elm because our code runs without runtime exceptions, but what if you have the wrong output? Even if we know what arguments the misbehaving function was passed to produce the bad output, we still have to start at both ends (the arguments and output) toward some point in between where something went wrong, often by inserting many log statements, recompiling, and re-running.

This project is an attempt to give total knowledge of the execution of an Elm function, so we can track down what went wrong, or just explore.

## Build & Run
```
make deps
make start
```
And go to `localhost:4000`.

## How it works

```
Elm source
=(compiler)=> JSON AST
=(tracing interpreter)=> Call Tree w/ TVals
=(elm-diagrams)=> Visualization
```


## Current state, future work

More detail in [the Trello](https://trello.com/b/6fNpWjix/reversible-interpreter#).

- interpreter
  - doesn't cover ADTs, records, or `case` expressions yet
  - doesn't even handle importing Elm code from other modules (almost there); doesn't yet reimplement things in `core` that are implemented with native code
- visualization
  - would like to link the flame graph, value viewer, and source code viewer together a lot more.
  - general usability & explorability…


## Practical challenges

- How to keep Elm AST types in sync with Haskell types (this may be useful for other projects though)
- How to fit this inside of the time-travelling debugger, or generally make it usable in more places

## Prior art

Total-recording debuggers are not a new idea; I can't remember any to cite and am not taking direct inspiration from any of them. The closest thing in my mind is Ravi Chugh's [Sketch-n-Sketch](https://github.com/ravichugh/sketch-n-sketch) (written in Elm, but with its own language), which uses interpreter traces to infer program changes from direct manipulation of graphical program output. (Much more advanced than this!)