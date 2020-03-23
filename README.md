[![Build Status](https://travis-ci.org/INCATools/boomer.svg?branch=master)](https://travis-ci.org/INCATools/boomer)

# boomer
Bayesian OWL ontology merging

## Running

boomer doesn't have releases or much of a command-line interface at the moment. Currently there is a `Main` class specialized for the term mapping use case. First, clone the repository and build the command-line package:

```bash
sbt stage
```

Then, run:

```bash
./target/universal/stage/bin/boomer slim-exposure-probs.tsv slim-exposure.owl true "http://purl.obolibrary.org/obo/ECTO http://purl.obolibrary.org/obo/NCIT http://purl.obolibrary.org/obo/MRE http://purl.obolibrary.org/obo/ZECO"
```

These are the expected arguments:
1. Term mapping p-table.
2. Background ontology.
3. `true` or `false`: shuffle mappings before sorting (will test different orders on different runs if there are many mappings with the same probability).
4. Space-separated list of term namespaces in which equivalences should be disallowed.
