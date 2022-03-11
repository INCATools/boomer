[![Build Status](https://travis-ci.org/INCATools/boomer.svg?branch=master)](https://travis-ci.org/INCATools/boomer)

# boomer
#### Bayesian OWL ontology merging

Boomer uses a combined logical and probabilistic approach to translate mappings into logical axioms that can be used to merge ontologies. Boomer implements a search algorithm to find the combined ontology with the highest probability that is also logically coherent. Boomer takes as input ontologies, plus mappings with probabilities for OWL interpretations for each mapping, and produces as output a set of cliques (each representing a group of overlapping mappings which interact with each other when selecting logical interpretations), together with a visualization of each clique. The resulting graphics provide a focused view into problematic tangles of mappings; this lends itself extremely well to an iterative inference process with a curator in the loop.

Boomer is implemented in Scala, and is built on the [Whelk OWL reasoner](https://github.com/balhoff/whelk). Whelk uses immutable data structures which allow Boomer to readily roll back to previous reasoning states in the course of its search algorithm. Boomer’s exhaustive search tests axiom combinations in order of decreasing joint probability. For larger cliques Boomer runs many depth-first “greedy” searches, with different starting points searched in parallel.

For some additional background, see this preprint describing an earlier implementation of the software: https://doi.org/10.1101/048843

## Usage

```
Usage: boomer [options]
  --usage  <bool>
        Print usage and exit
  --help | -h  <bool>
        Print help message and exit
  --output | -o  <output files/dir name>
        Name used for folder to ouput clique JSON files; also basename for ontology and Markdown output files.
  --ptable | -t  <filename>
        TSV file containing table of mappings with probabilities.
  --ontology | -a  <filename>
        OWL file containing all asserted/background axioms.
  --prefixes | -p  <filename>
        YAML dictionary of prefix-to-expansion mappings for all prefixes used in the ptable. These namespaces are also used to check for new within-namespace equivalences.
  --window-count | -w  <positive integer>
        Number of groups to split a clique of mappings into when shuffling between greedy search runs. Windows maintain their order; mappings within a window are shuffled.
  --runs | -r  <positive integer>
        Number of separate shuffled runs to conduct for each greedy search.
  --exhaustive-search-limit | -e  <positive integer>
        Maximum size clique for exhaustive search algorithm. Larger cliques use greedy search.
  --output-internal-axioms | -e  <bool>
        Include axioms used to enforce proper subclass relationships (e.g. generated disjoint sibling classes) in OWL output (default false).
  --restrict-output-to-prefixes | -e  <prefix strings (max of 2)>
        Generate output only for cliques where a mapping between these two namespaces was resolved as something other than its highest probability option.
```

## Running

A pre-built copy of Boomer can downloaded from the [releases page](https://github.com/INCATools/boomer/releases), e.g., `https://github.com/INCATools/boomer/releases/download/v0.1/boomer-0.1.tgz`. After unzipping the archive you should see a `bin` and a `lib` folder. Keep these together in the same folder, and add the `bin` folder to your path.

The current command-line interface is specialized for the term mapping use case (see options above).

To run Boomer, you must have Java installed. To set the JVM heap size (usually necessary for processing larger files), use an environment variable:

```bash
export JAVA_OPTS=-Xmx10G
```

## Example command line (from https://github.com/geneontology/go-rhea-boom/blob/master/Makefile)

```bash
boomer --ptable probs.tsv --ontology go-rhea.ofn --window-count 1 --runs 100 --prefixes prefixes.yaml --output rhea-boom --exhaustive-search-limit 14 --restrict-output-to-prefixes=GO --restrict-output-to-prefixes=RHEA
```

## Building

If you want to build the code yourself, you must first install [`sbt`](https://www.scala-sbt.org). Clone the repository and run `sbt` in the project root folder. A few SBT commands will be useful:

- `compile`: build the code
- `test`: run all tests
- `stage`: create an executable for local use at `./target/universal/stage/bin/boomer`
- `packageZipTarball`: package the executable for release

