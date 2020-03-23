[![Build Status](https://travis-ci.org/INCATools/boomer.svg?branch=master)](https://travis-ci.org/INCATools/boomer)

# boomer
Bayesian OWL ontology merging

## Running

`boomer` doesn't have releases or much of a command-line interface at the moment. Currently there is a `Main` class specialized for the term mapping use case. First, clone the repository and build the command-line package:

```bash
sbt stage
```
This will create the executable `./target/universal/stage/bin/boomer`.  

To run, `boomer` expects 4 arguments:
1. A tsv file containing the probabilities of each IRI/term.
2. Background ontology.
3. `true` or `false`: 
`true` (default) will shuffle mappings before sorting. This tests different orders on different runs if there are many mappings with the same probability.  
`false` will not shuffle mappings.
4. Space-separated list of term namespaces in which equivalences should be disallowed.

The output is saved in file `output.txt` and contains a table of mappings between the backround ontology and the classes/terms specified in the probabilities file.

## Example
This command uses the `slim-exposure-probs.tsv` probability file and `slim-exposure-probs.owl` ontology file to produce mappings between classes the `slim-exposure-probs.owl` ontology and other ontology terms specified in the `slim-exposure-probs.tsv` file.  

```bash
./target/universal/stage/bin/boomer slim-exposure-probs.tsv slim-exposure.owl true "http://purl.obolibrary.org/obo/ECTO http://purl.obolibrary.org/obo/NCIT http://purl.obolibrary.org/obo/MRE http://purl.obolibrary.org/obo/ZECO"
```
The output file looks like this:
```
ECTO:0001606 SiblingOf Wikidata:Q21167711       false
ECTO:0002110 EquivalentTo Wikidata:Q21173580    true
ECTO:0001628 SiblingOf NCIT:C45558      false
ECTO:0001750 EquivalentTo NCIT:C29821   true
ECTO:0002115 EquivalentTo Wikidata:Q21174350    true
```
The `true`/`false` designate wheter the highest probability value was used.

