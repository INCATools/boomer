[![Build Status](https://travis-ci.org/INCATools/boomer.svg?branch=master)](https://travis-ci.org/INCATools/boomer)

# boomer
#### Bayesian OWL ontology merging

Boomer uses a combined logical and probabilistic approach to translate mappings into logical axioms that can be used to merge ontologies. Boomer implements a search algorithm to find the combined ontology with the highest probability that is also logically coherent. Boomer takes as input ontologies, plus mappings with probabilities for OWL interpretations for each mapping, and produces as output a set of cliques (each representing a group of overlapping mappings which interact with each other when selecting logical interpretations), together with a visualization of each clique. The resulting graphics provide a focused view into problematic tangles of mappings; this lends itself extremely well to an iterative inference process with a curator in the loop.

Boomer is implemented in Scala, and is built on the [Whelk OWL reasoner](https://github.com/balhoff/whelk). Whelk uses immutable data structures which allow Boomer to readily roll back to previous reasoning states in the course of its search algorithm. Boomer’s exhaustive search tests axiom combinations in order of decreasing joint probability. For larger cliques Boomer runs many depth-first “greedy” searches, with different starting points searched in parallel.

For some additional background, see this preprint describing an earlier implementation of the software: https://doi.org/10.1101/048843



## Building

If you want to build the code yourself, you must first install [`sbt`](https://www.scala-sbt.org). Clone the repository and run `sbt` in the project root folder. A few SBT commands will be useful:

- `compile`: build the code
- `test`: run all tests
- `stage`: create an executable for local use at `./target/universal/stage/bin/boomer`
- `packageZipTarball`: package the executable for release

