# brainf--k

The smallest Haskell brainf--k interpreter known to man (maybe).

Usage:

```
brainfuck [FILE]
```

The `./examples` directory contains one of my favorite pieces of code ever: a pseudo-random number generator in pure brainfuck using the Rule 30 automaton, written by Daniel B Cristofani.

Run

```
brainfuck examples/random.b
```

for a really slow non-terminating stream of psuedo-random bytes to stdout.
If you want to change the seed, you have to go into the brainf--k file and mess with the initalization code.

