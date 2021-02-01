# flip

given two ontologies:

```
 A1   B1
 ^    ^
 |    |
 |    |
 A2   B2
```

and 'criss-crossing' mappings between A1-B2 and A2-B1, i.e

```
 A1   B1
 ^\   /^
 | \ / |
 | / \ |
 A2   B2
```

if these mappings have a high probability of being EquivalentClasses, we do not allow both to be true, as this results in an equivalence clique, and we assign Pr(x Equiv y)=0.0 if x and y are from the same prefix.

instead an arbitrary axiom is treated as being equivalent, and the other is inferred to be subClassOf (proper), i.e. the output would be:

```
A:2 ProperSubClassOf B:1                0.01
A:1 EquivalentTo B:2    (most probable) 0.95
```

The existence of axioms that are not the most probably - in particular ones with such low priors - is a sign the curator should check here, maybe there is an error in a source ontology.
