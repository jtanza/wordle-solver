# A Wordle Solver

Provides guesses for the excellent [Wordle](https://www.powerlanguage.co.uk/wordle/) puzzle game.

Mainly built to annoy my wife in our daily competitions to solve wordles, but also to learn some OCaml.

# Implementation

Builds a trie from frequently used [english words](https://www.kaggle.com/rtatman/english-word-frequency) performing wildcard 
searches against candidates built from the user's past puzzle guesses.


# Install

```
λ ~/ git clone https://github.com/jtanza/wordle-solver.git && cd wordle-solver
λ ~/wordle-solver/ dune build wordle_solver.exe && ./_build/default/wordle_solver.exe
```

# Usage

```
λ ~/wordle-solver/ ./_build/default/wordle_solver.exe --help

Provides guesses for Wordle puzzles by prompting users for previously placed letters and their green/gray/yellow outcome.

For green and yellow prompts, enter a comma separated list of letters and the 0 based index of their last placement, e.g. d2,s4
For gray prompts, enter a comma separated list of letters only, e.g. x,y,z
Simply omit any input if no letters of a particualr color were matched.
```

# Example

Guess progression `saint -> print -> point`

[![asciicast](https://asciinema.org/a/qb8kKSEBLmktsFdAd361vTYWT.svg)](https://asciinema.org/a/qb8kKSEBLmktsFdAd361vTYWT)


