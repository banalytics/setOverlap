This repository contains reusable parts of the code used for comparing different set visualization in my blog post [futureurl]
It contains 5 files:

dataGeneration.R: Generates dummy data used in blogpost

combinationCalculation.R: Recursively calculates all possible set configurations and their frequencies based (overlaps as well as single sets)

vennDiagramPackageWrappers.R: Wrappers for different functions from VennDiagram to make them easier to use

overlapCorrelation.R: Calculation of overlap only correlation

overlapShares.R: Calculation of pairwise two-way overlap shares )

TODO:
Add input checks into all functions
Improve Input / Output documentation
Possibly turn into an R package