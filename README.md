# Project: permutation1

Project for developing a permutation function.

## Team setup

Adrián:
- Project Manager

Solveig:
- Documentation execute
- Quality executive

## Explaination of the project:

We want to make a package, that can make permutations for data, that needs a non-parametric method. It needs to be able to permute a dataset and compare observed statistical value with permuted values.In addition, it needs to be able to make a different number of permutations conditional on what is chosen.
We want it to be able to calculate different statistical values for making a p-value depending on what is chosen for the dataset.Moreover,we want it to compute a p-value, that tells us how likely our observed statistical value is. Also, we want a function to plot our permuted values nicely with the observed statistical value to get a nice graphical representation of it and get a visual idea of how significant it is. Finally, We want it to be able to check, if the data can be used in a parametric test, then we want to break the function and say that permutation takes "this" many iterations (lot of time) and that it might be better to do a parametric test.

## Roadmap:
### gamma:
	1) Create permuation algorithm (permutation + p-value + plot)
	2) Minimum interface + Docs

### beta:
	3) Develop functionalities + classes
	4) Create package + Docs
	5) Testing
