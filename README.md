[![Build Status](https://travis-ci.org/INCATools/boomer.svg?branch=master)](https://travis-ci.org/INCATools/boomer)

# boomer
Bayesian OWL ontology merging

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

`boomer` doesn't have releases or much of a command-line interface at the moment. Currently there is a `Main` class specialized for the term mapping use case. First, clone the repository and build the command-line package:

```bash
sbt stage
```
This will create the executable `./target/universal/stage/bin/boomer`.

To set the JVM heap size, use an environment variable:

```bash
export JAVA_OPTS=-Xmx10G
```

To run, `boomer` expects 5 arguments:
- `ptable`: A tsv file containing the probabilities of each IRI/term.
- `ontology`: Background ontology. 
- `prefixes`: A YAML file of term namespaces, used for expanding IDs in the probability table, and also for specifying namespaces in which equivalences should be disallowed.
- `window-count`: Number of bins to divide the probability range into. Mappings within each bin will be shuffled on each run.
- `runs`: Number of searches for coherent axiom configuration to conduct.

Three output files are produced:
- `output.txt`: List of chosen mapping configurations in the most probable run.
- `output-hotspots.txt`: List of mappings in which multiple configurations were chosen in different runs.
- `output.ofn`: OWL ontology containing mapping axioms selected in the most probable run.

## Example
This command uses the `slim-exposure-probs.tsv` probability file and `slim-exposure-probs.owl` ontology file to produce mappings between classes the `slim-exposure-probs.owl` ontology and other ontology terms specified in the `slim-exposure-probs.tsv` file.  

```bash
./target/universal/stage/bin/boomer --ptable slim-exposure-probs.tsv --ontology slim-exposure.owl --window-count 10 --runs 100 --prefixes prefixes.yaml --output exposo
```

The `output.txt` file looks like this:

```
ECTO:0001606 SiblingOf Wikidata:Q21167711       false
ECTO:0002110 EquivalentTo Wikidata:Q21173580    true
ECTO:0001628 SiblingOf NCIT:C45558      false
ECTO:0001750 EquivalentTo NCIT:C29821   true
ECTO:0002115 EquivalentTo Wikidata:Q21174350    true
```
The `true`/`false` designate whether the highest probability value was used.

