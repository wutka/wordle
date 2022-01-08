# A Wordle Solver

This is a simple interactive program that tries to solve a
[Wordle](https://www.powerlanguage.co.uk/wordle/) puzzle.

The program prints out a word to try in Wordle, and then you have
to tell it how Wordle responded. For each word, Wordle will color
a letter green if it is in the right place, yellow if a letter is
in the word but not in the right place, or gray if the letter is
not in the word. To tell the solver the results, you enter . for
a gray letter, upper-case for green, and lower-case for yellow.

For example, the solver first wants to try AROSE (currently hard-coded
as that's what it computed as the best first try from the dictionary
it uses), and for the Wordle on Jan. 7, 2022, the S was yellow, and
the other letters gray, so I entered `...s.`. The solver then asked
me to try UNITS and Wordle had U and S in yellow, and NIT in gray,
so I entered `u...s` at which point its third guess was correct.
But, if N had been green, I would have entered `uN..s`.

## Running
The program is written in Haskell and uses Cabal for building.
You should be able to run it with:
```
cabal run
```
