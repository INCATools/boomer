[![Build Status](https://travis-ci.org/INCATools/boomer.svg?branch=master)](https://travis-ci.org/INCATools/boomer)

# boomer
Bayesian OWL ontology merging

## Running

`boomer` doesn't have releases or much of a command-line interface at the moment. Currently there is a `Main` class specialized for the term mapping use case. First, clone the repository and build the command-line package:

```bash
sbt stage
```
This will create the executable `./target/universal/stage/bin/boomer`.  

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
./target/universal/stage/bin/boomer --ptable slim-exposure-probs.tsv --ontology slim-exposure.owl --window-count 10 --runs 20 --prefixes prefixes.yaml
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

