# example of being forced to select a non-optimal prior

Given:

 1. Pr(A properSubClassOf C) = 0.99
 2. Pr(A equiv B) = 0.95
 3. Pr(B equiv C) = 0.95

(in each case, the only other possibility is siblingOf)

Solutions:

 * 1,2,3 : incoherent
 * 1,2   : .99 * .95 * (1-.95) = 0.04
 * 1,3   : .99 * .95 * (1-.95) = 0.04
 * 2,3   : .95 * .95 * (1-0.99) = 0.009
 * 1     : .99 * .05 * .05 = 0.0023
 * 2     : .01 * .95 * .05 = 0.000475
 * 3     : .01 * .95 * .05 = 0.000475
 * {}    : .01 * .05 * .05 = 2.5e-05
 
 
