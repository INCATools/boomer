# Boomerdle

NOT WORKING

aims to show how boomer can be used to find most likely Wordle answers.

## Background knowledge

[letter-freqs.csv](letter-freqs.csv):

```
A, 0.05, 0.05, 0.05, 0.05, 0.05
B, 0.05, 0.05, 0.05, 0.05, 0.05
C, 0.05, 0.05, 0.05, 0.05, 0.05
```

this is the prior chance of a letter being present at positions 1-5. Currently seeded at equal probs but this could be changed, e.g. Q lower.

probabilities of pairwise, e.g Q-followed-by-not-U is not supported

This is compiled to a ptable [letters.ptable.tsv](letters.ptable.tsv) by [make-ptable-from-words.pl](make-ptable-from-words.pl)

```
✗ less word-probs.csv 
bunch, 0.001
canoe, 0.001
stair, 0.001
stare, 0.001
dance, 0.001
zebra, 0.0005
```

this is just a demo set. Could be expanded using any dictionary plus info on frequencies

This is converted to words.ptable.tsv

## Logical Axioms

### Background Knowledge

[wordle.omn](wordle.omn)

### Guesses so far

These go in:

my-guesses-N.csv

the 2nd colum is the wordle feedback (G=green, O=organe)

These are translated into logical axioms (combined with the list of all words)

canoe, OxGxx

=>

```owl
Class: soln:canoe DisjointWith: :TodaysWord
Class: :TodaysWord SubClassOf: :hasLetter some :C
Class: :TodaysWord DisjointWith: :hasLetter1 some :C
Class: :TodaysWord DisjointWith: :hasLetter some :A
Class: :TodaysWord SubClassOf: :hasLetter3 some :N
Class: :TodaysWord DisjointWith: :hasLetter some :O
Class: :TodaysWord DisjointWith: :hasLetter some :E
Class: soln:bunch
Class: soln:canoe
Class: soln:stair
Class: soln:stare
Class: soln:dance
Class: soln:zebra
Class: :TodaysWord DisjointUnionOf: soln:bunch, soln:canoe, soln:stair, soln:stare, soln:dance, soln:zebra
Class: :TodaysWord SubClassOf: soln:bunch OR soln:canoe OR soln:stair OR soln:stare OR soln:dance OR soln:zebra
```

## Boomer

all ptables and owl is combined and boomer is run

currently Whelk doesn't support DisjointUnionOf so we can't 'close the world', and boomer gives the most likely answer
as being *none* of the candidate words, so it refuses to play and pick one (not unlike the computer in wargames).
