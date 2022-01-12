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

The program uses a dictionary with word frequencies that is an intersection
of a list of common words, and Peter Norvig's [count_1w.txt](https://norvig.com/ngrams/count_1w.txt) file with word
frequences of English words. If a word was in my list of common words but not 
in Norvig's list, I assigned it a frequency of 1.

## Running
The program is written in Haskell and uses Cabal for building.
You should be able to run it with:
```
cabal run
```

## Strategy
Whenever the program makes a guess, it looks at the remaining words and
counts the total letter frequencies for the whole list. Then it looks for
the word that has the highest score if you add up the frequency counts of each
unique letter in the word (so the score for FALL would be the frequencies of 
F, A, and L, but only counting L once). Most recently, it now uses the frequency
of the word to break ties in the letter frequency score. This likely only happens
when you get down to a few words, but it might help it pick a more common word
over a more obscure one.

Since Wordle is currently 5 letters, the program will always start with AROSE.
While the letter frequencies for English text have ETAONIRSH as the most frequent 
letters, much of that comes from the interplay of various words. If you just do a
letter count of the 5 letter words, at least in the dictionary I am using, which
tried to stick to common words, the most frequent letters are SEAROLTINDU. AROSE 
is the only word in my dictionary that has the five most frequent letters.

Picking the best candidate word is the only strategic part of the program, the other
processing is fairly manual. Once it gets the feedback from Wordle as to which letters
are correct & incorrect, rightly placed or not, the program creates a list of 
restrictions based on that feedback. The four types of restrictions are:

1. MustContain - any candidate word must contain all the letters in the MustContain list
2. CannotContain - any candidate word cannot contain any letter in the CannotContain list
3. Positions - any candidate word must have a specific letter at a specific position
4. NotPositions - any candidate word cannot have some letter x at some position y

The program starts with a full dictionary of 5 letters (you can change the word length
with "--length n" on the command-line), and picks the word with the best letter frequencies.
When it receives the feedback from Wordle, it parses the response into lists of 
MustContain, CannotContain, Positions, and NotPositions and adds those to the existing
(initially empty) lists for each type of restriction. Then, creates a new subset of the
dictionary containing only those words that match all the restrictions. When it gets
down to only 1 word in the dictionary it quits, declaring it to be the final word.
